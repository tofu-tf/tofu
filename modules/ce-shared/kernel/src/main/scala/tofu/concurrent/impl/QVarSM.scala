package tofu.concurrent.impl

import cats.Monad
import tofu.concurrent.QVar
import tofu.syntax.monadic._
import cats.data.{NonEmptyVector => NEV}
import scala.annotation.tailrec

abstract class QVarSM[F[_]: Monad, A, P] extends QVar[F, A] {
  import QVarSM._

  protected type S = State[A, P]
  protected def newPromise: F[P]
  protected def complete(x: A, promise: P): F[Unit]
  protected def await(p: P): F[A]
  protected def modifyF[X](f: S => (S, F[X])): F[X]
  protected def get: F[S]
  protected def set(s: S): F[Unit]
  protected def onCancel(fa: F[A], fc: => F[Unit]): F[A]

  override def isEmpty = get.map {
    case Contains(p, _) => p.isEmpty
    case _              => true
  }

  // we use manual queue of requests here with controllable offset
  // offset is an offset of a permanent request index in our pseudo-queue,
  // needed by the take operation, too keep track its position
  // every time put removes a head request it will increment the offset
  override def put(a: A): F[Unit] = modifyF {
    case Contains(v, off) => (Contains(a +: v, off), unit[F])
    case Await(reqs, off) => putIter(reqs.toVector, off, a, unit[F])
  }

  @tailrec private[this] def putIter(reqs: Vector[Request[P]], offset: Long, a: A, fa: F[Unit]): (S, F[Unit]) =
    reqs match {
      case Read(p) +: rest           => putIter(rest, offset + 1, a, fa *> complete(a, p))
      case Take(p) +: (next +: rest) => (Await(NEV(next, rest), offset + 1), fa *> complete(a, p))
      case Take(p) +: _              => (Contains(offset = offset + 1), fa *> complete(a, p))
      case empty @ _                 => (Contains(Vector(a), offset), fa)
    }

  override def read: F[A] = modifyF {
    case s @ Contains(a +: _, _)    => (s, a.pure[F])
    case s @ Contains(empty @ _, _) => (s, newPromise >>= retryRead)
    case s: Await[P]                => (s, await(s.reqs.head.promise))
  }

  // same as read but has promise to make other readers wait
  private[this] def retryRead(promise: P): F[A] = modifyF {
    case s @ Contains(a +: _, _) => (s, a.pure[F])
    case Contains(_, off)        => (Await(NEV.one(Read(promise)), off), await(promise))
    case s: Await[P]             => (s, await(s.reqs.head.promise))
  }

  // on take cancelation we will use our best effort
  // to convert corresponding request to Read,
  // so put will complete it and go forward
  override def take: F[A] = modifyF {
    case Contains(a +: rest, _)     => (Contains(rest), a.pure[F])
    case s @ Contains(empty @ _, _) => (s, newPromise >>= retryTake)
    case s @ Await(reqs, off)       =>
      reqs.last match {
        case Read(p) =>
          (Await(NEV.fromVectorUnsafe(reqs.init :+ Take(p)), off), awaitTake(p, off + reqs.length - 1))
        case Take(_) => (s, newPromise >>= retryTake)
      }
  }

  private[this] def awaitTake(promise: P, index: Long) = onCancel(await(promise), revertTake(index))

  private[this] def retryTake(promise: P): F[A] = modifyF {
    case Contains(a +: rest, _)      => (Contains(rest), a.pure[F])
    case Contains(empty @ _, offset) => (Await(NEV.one(Take(promise)), offset), awaitTake(promise, offset))
    case Await(reqs, off)            =>
      reqs.last match {
        case Read(p) => (Await(NEV.fromVectorUnsafe(reqs.init :+ Take(p)), off), awaitTake(p, off + reqs.length - 1))
        case Take(_) => (Await(reqs :+ Take(promise)), awaitTake(promise, off + reqs.length))
      }
  }

  private[this] def update(f: S => S): F[Unit] = modifyF(s => (f(s), unit[F]))

  private[this] def revertTake(index: Long): F[Unit] = update {
    case s @ Await(reqs, offset) if offset <= index || reqs.length + offset > index =>
      val k = (index - offset).toInt
      reqs.getUnsafe(k) match {
        case Take(p) => Await(reqs.updatedUnsafe(k, Read(p)), offset)
        case _       => s
      }
    case s                                                                          => s
  }
}

object QVarSM {
  def fromOption[A](opt: Option[A]): State[A, Nothing] = Contains(opt.toVector)

  sealed trait Request[+P] {
    def promise: P
  }
  final case class Read[+P](promise: P) extends Request[P]
  final case class Take[+P](promise: P) extends Request[P]

  private[QVarSM] val empty = Contains()

  sealed trait State[+A, +P]

  // there is some value
  case class Contains[+A](values: Vector[A] = Vector.empty, offset: Long = 0) extends State[A, Nothing]

  // there is no value and some readers and takers want the next value
  final case class Await[+P](reqs: NEV[Request[P]], offset: Long = 0) extends State[Nothing, P]
}
