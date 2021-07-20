package tofu.interop

import cats.Monad
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import tofu.concurrent.QVar
import tofu.syntax.monadic._

//this implementation is really questionable
final case class QVarQRef[F[_], A](puts: Queue[F, A], ref: Ref[F, Option[A]])(implicit F: Monad[F]) extends QVar[F, A] {
  override def isEmpty: F[Boolean] = ref.get.map(_.isEmpty)
  override def put(a: A): F[Unit]  =
    ref.modify {
      case s @ Some(_) => (s, puts.offer(a))
      case None        => (Some(a), F.unit)
    }.flatten

  override def take: F[A] =
    ref.modify {
      case Some(a) => (None, F.pure(a))
      case None    => (None, puts.take)
    }.flatten
  override def read: F[A] = ref.get.flatMap {
    case None    => puts.take.flatTap(puts.offer)
    case Some(a) => F.pure(a)
  }
}
