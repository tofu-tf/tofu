package tofu.sim
import Transact._
import cats.effect.concurrent.MVar
import tofu.sim.SIM.{IO, IOMonad, STM, STMMonad, TVAR}
import tofu.syntax.monadic._
import cats.syntax.foldable._
import cats.instances.option._

case class SimMVar[F[_, _]: IOMonad: STMMonad: Transact, A](tvar: F[TVAR, Option[A]]) extends MVar[F[IO, *], A] {
  def isEmpty: F[IO, Boolean] = tvar.read.map(_.isEmpty).atomically
  def put(a: A): F[IO, Unit] = tvar.read.flatMap {
    case None => tvar.write(Some(a))
    case _    => fail[F, Unit]
  }.atomically
  def tryPut(a: A): F[IO, Boolean] = tvar.read.flatMap {
    case None => tvar.write(Some(a)) as true
    case _    => false.pure[F[STM, *]]
  }.atomically

  def take: F[IO, A] = tvar.read.flatMap {
    case None    => fail[F, A]
    case Some(a) => tvar.write(None) as a
  }.atomically
  def tryTake: F[IO, Option[A]] = tvar.read.flatTap { _.traverse_(_ => tvar.write(None)) }.atomically
  def read: F[IO, A] = tvar.read.flatMap {
    case None    => fail[F, A]
    case Some(a) => a.pure[F[STM, *]]
  }.atomically

}
