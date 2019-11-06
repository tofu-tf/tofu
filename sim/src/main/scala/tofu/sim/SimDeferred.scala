package tofu.sim
import cats.effect.concurrent.Deferred
import tofu.sim.SIM.{IO, IOMonad, STM, STMMonad, TVAR}
import tofu.syntax.monadic._
import Transact._

case class SimDeferred[F[_, _]: IOMonad: STMMonad: Transact, A](tvar: F[TVAR, Option[A]])
    extends Deferred[F[IO, *], A] {
  def get: F[IO, A] = tvar.read.flatMap {
    case None    => fail[F, A]
    case Some(a) => a.pureSTM[F]
  }.atomically

  def complete(a: A): F[IO, Unit] = tvar.read.flatMap {
    case None => tvar.write(Some(a)) as false
    case _    => true.pureSTM[F]
  }.atomically >>=
    panic[F, Unit]("Attempting to complete a Deferred that has already been completed").whenA
}
