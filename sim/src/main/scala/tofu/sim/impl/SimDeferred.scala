package tofu.sim
import cats.effect.concurrent.Deferred
import tofu.sim.SIM._
import tofu.syntax.monadic._
import Transact._

case class SimDeferred[F[_, _]: IOMonad[*[_, _], E]: STMMonad: Transact, E, A](tvar: F[TVAR, Option[A]])
    extends Deferred[F[RUN[E], *], A] {
  def get: F[RUN[E], A] = tvar.read.flatMap {
    case None    => fail[F, A]
    case Some(a) => a.pureSTM[F]
  }.atomically

  def complete(a: A): F[RUN[E], Unit] = tvar.read.flatMap {
    case None => tvar.write(Some(a)) as false
    case _    => true.pureSTM[F]
  }.atomically[E] >>=
    panic[F, E, Unit]("Attempting to complete a Deferred that has already been completed").whenA
}
