package tofu.concurrent.impl

import cats.Monad
import tofu.concurrent.QVar
import tofu.syntax.monadic._

abstract class QVarQueue[F[_]: Monad, A, Promise] extends QVar[F, A] {
  import QVarQueue._

  protected type S = State[A, Promise]
  protected def newPromise: F[Promise]
  protected def complete(x: A, p: Promise): F[Unit]
  protected def await(p: Promise): F[A]
  protected def modifyUncancelableF[X](f: S => (Boolean, S, F[X])): F[X]
  protected def modifyF[X](f: S => (S, F[X])): F[X]
  protected def set(s: S): F[Unit]
  protected def offer(a: A): F[Unit]
  protected def qtake: F[A]
  protected def tryOffer(a: A): F[Boolean]
  protected def uncancellable[X](fx: F[X]): F[X]

  override def put(a: A): F[Unit] = modifyF {
    case s @ Full(_)                => (s, offer(a))
    case AwaitTake(p)               => (Empty, complete(a, p))
    // lazily initialize new promise to block other putters
    case s @ (Empty | AwaitRead(_)) => (s, newPromise >>= retryPut(a))
    // we block the putter here to make the order of takes and reads consistent
    case s @ AwaitPut(p)            => (s, await(p) >> put(a))
  }

  // same as put but has promise to make other putters wait
  private[this] def retryPut(a: A)(promise: Promise): F[Unit] = modifyUncancelableF {
    case s @ Full(_)     => (false, s, offer(a))
    // there could be still some takers in the queue
    case Empty           => (true, AwaitPut(promise), tryOffer(a).ifM(set(Empty), set(Full(a)) >> complete(a, promise)))
    case AwaitRead(r)    =>
      (true, AwaitPut(promise), uncancellable(complete(a, r) >> set(Full(a)) >> complete(a, promise)))
    // directly complete current taker desire
    case AwaitTake(p)    => (false, Empty, complete(a, p))
    // we will reuse this promise just in case someone would waiting to read again
    case s @ AwaitPut(p) => (false, s, await(p) >> retryPut(a)(promise))
  }

  override def read: F[A] = modifyF {
    case s @ Full(a)      => (s, a.pure[F])
    case s @ AwaitRead(p) => (s, await(p))
    case s @ AwaitPut(p)  => (s, await(p))
    case s @ Empty        => (s, newPromise >>= retryRead)
    case s @ AwaitTake(p) => (s, await(p))
  }

  //same as read but has promise to make other readers wait
  private[this] def retryRead(promise: Promise): F[A] = modifyF {
    case s @ AwaitRead(p) => (s, await(p))
    case s @ AwaitTake(p) => (s, await(p))
    case s @ AwaitPut(p)  => (s, await(p))
    case s @ Full(a)      => (s, a.pure[F])
    case Empty            => (AwaitRead(promise), await(promise))
  }

  override def take: F[A] = modifyF {
    case Full(a)          => (Empty, a.pure)
    case s @ Empty        => (s, newPromise >>= retryTake)
    case AwaitRead(p)     => (AwaitTake(p), await(p))
    case s @ AwaitPut(p)  => (s, await(p) >> take)
    case s @ AwaitTake(_) => (s, qtake)
  }

  private[this] def retryTake(promise: Promise): F[A] = modifyF {
    case Full(a)          => (Empty, a.pure)
    case Empty            => (AwaitTake(promise), await(promise))
    case AwaitRead(p)     => (AwaitTake(p), await(p))
    case s @ AwaitPut(p)  => (s, await(p) >> retryTake(promise))
    case s @ AwaitTake(_) => (s, qtake)
  }
}

object QVarQueue {
  sealed trait State[+A, +Promise]
  // there is some value
  case class Full[+A](a: A)                        extends State[A, Nothing]
  // there is no value
  case object Empty                                extends State[Nothing, Nothing]
  // there is no value and some readers want the next value
  final case class AwaitRead[+Promise](p: Promise) extends State[Nothing, Promise]
  // there is no value and a taker wants the next value
  final case class AwaitTake[+Promise](p: Promise) extends State[Nothing, Promise]
  // there is some putter wanting to push value and notify readers
  final case class AwaitPut[+Promise](p: Promise)  extends State[Nothing, Promise]
}
