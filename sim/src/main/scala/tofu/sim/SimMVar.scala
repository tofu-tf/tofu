package tofu.sim
import Transact._
import cats.effect.concurrent.MVar
import tofu.sim.SIM.{RUN, IOMonad, STM, STMMonad, TVAR}
import tofu.syntax.monadic._
import cats.syntax.foldable._
import cats.instances.option._

case class SimMVar[F[_, _]: IOMonad[*[_, _], E]: STMMonad: Transact,E, A](tvar: F[TVAR, Option[A]]) extends MVar[F[RUN[E], *], A] {
  def isEmpty: F[RUN[E], Boolean] = tvar.read.map(_.isEmpty).atomically
  def put(a: A): F[RUN[E], Unit] = tvar.read.flatMap {
    case None => tvar.write(Some(a))
    case _    => fail[F, Unit]
  }.atomically
  def tryPut(a: A): F[RUN[E], Boolean] = tvar.read.flatMap {
    case None => tvar.write(Some(a)) as true
    case _    => false.pure[F[STM, *]]
  }.atomically

  def take: F[RUN[E], A] = tvar.read.flatMap {
    case None    => fail[F, A]
    case Some(a) => tvar.write(None) as a
  }.atomically
  def tryTake: F[RUN[E], Option[A]] = tvar.read.flatTap { _.traverse_(_ => tvar.write(None)) }.atomically
  def read: F[RUN[E], A] = tvar.read.flatMap {
    case None    => fail[F, A]
    case Some(a) => a.pure[F[STM, *]]
  }.atomically

}
