package tofu.concurrent.impl

import cats.Monad
import tofu.concurrent.QVar
import tofu.syntax.monadic._
import tofu.syntax.selective._

abstract class QVarSM[F[_]: Monad, A, P] extends QVar[F, A] {
  import QVarSM._

  protected type S = State[A, P]
  protected def newPromise: F[P]
  protected def complete(x: A, promise: P): F[Boolean]
  protected def await(p: P): F[A]
  protected def awaitOrCancel(p: P): F[A]
  protected def modifyF[X](f: S => (S, F[X])): F[X]
  protected def get: F[S]
  protected def set(s: S): F[Unit]

  override def isEmpty = get.map {
    case Contains(p) => p.isEmpty
    case _           => true
  }

  override def put(a: A): F[Unit] = modifyF {
    case Contains(v) => (Contains(a +: v), unit[F])
    case s: Await[P] => (putNext(s, a), complete(a, s.promise).unlesss_(put(a)))
  }

  private[this] def putNext(s: Await[P], a: A) = s match {
    case AwaitRead(_)               => Contains(Vector(a))
    case AwaitTake(_, next +: rest) => AwaitTake(next, rest)
    case AwaitTake(_, _)            => empty
  }

  override def read: F[A] = modifyF {
    case s @ Contains(a +: _) => (s, a.pure[F])
    case s @ Contains(_)      => (s, newPromise >>= retryRead)
    case s: Await[P]          => (s, await(s.promise))
  }

  //same as read but has promise to make other readers wait
  private[this] def retryRead(promise: P): F[A] = modifyF {
    case s @ Contains(a +: _) => (s, a.pure[F])
    case Contains(_)          => (AwaitRead(promise), await(promise))
    case s: Await[P]          => (s, await(s.promise))
  }

  override def take: F[A] = modifyF {
    case Contains(a +: rest) => (Contains(rest), a.pure[F])
    case AwaitRead(p)        => (AwaitTake(p), await(p))
    case s                   => (s, newPromise >>= retryTake)
  }

  private[this] def retryTake(promise: P): F[A] = modifyF {
    case Contains(a +: rest) => (Contains(rest), a.pure[F])
    case Contains(_)         => (AwaitTake(promise), awaitOrCancel(promise))
    case AwaitRead(p)        => (AwaitTake(p), await(p))
    case AwaitTake(p, rest)  => (AwaitTake(p, rest :+ promise), awaitOrCancel(promise))
  }
}

object QVarSM {
  def fromOption[A](opt: Option[A]): State[A, Nothing] = Contains(opt.toVector)

  private[QVarSM] val empty = Contains()

  sealed trait State[+A, +P]

  sealed trait Await[+P]                                                     extends State[Nothing, P] {
    def promise: P
    def rest: Vector[P]
  }
  // there is some value
  case class Contains[+A](values: Vector[A] = Vector.empty)                  extends State[A, Nothing]
  // there is no value and some readers want the next value
  final case class AwaitRead[+P](promise: P)                                 extends Await[P]          {
    def rest = Vector.empty
  }
  // there is no value and some takers want the next value
  final case class AwaitTake[+P](promise: P, rest: Vector[P] = Vector.empty) extends Await[P]
}
