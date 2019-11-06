package tofu.sim
package mutable
import cats.data.{EitherT, OptionT}
import cats.{Monad, MonadError, StackSafeMonad}
import tofu.sim.SIM.{IO, STM, TVAR}
import tofu.sim.mutable.MutSim.{MutIO, MutSTM, MutVar}

sealed trait MutSim[E, T, A]

object MutSim {
  final case class MutIO[E, A](fa: SimIO[E, A]) extends MutSim[E, IO, A]
  final case class MutSTM[E, A](fa: SimT[A])    extends MutSim[E, STM, A]
  final case class MutVar[E, A](va: MutTVar[A]) extends MutSim[E, TVAR, A]

  implicit class IOOPs[E, A](private val sim: MutSim[E, IO, A]) extends AnyVal {
    def value: SimIO[E, A] = sim match { case MutIO(fa) => fa }
  }
  implicit class STMOps[E, A](private val sim: MutSim[E, STM, A]) extends AnyVal {
    def value: SimT[A] = sim match { case MutSTM(fa) => fa }
  }
  implicit class TVarOps[E, A](private val sim: MutSim[E, TVAR, A]) extends AnyVal {
    def value: MutTVar[A] = sim match { case MutVar(fa) => fa }
  }

  implicit def transact[E]: Transact[MutSim[E, *, *]]      = new MutSimTransact[E]
  implicit def ioMonad[E]: MonadError[MutSim[E, IO, *], E] = new MutSimIOMonad[E]
  implicit def stmMonad[E]: Monad[MutSim[E, STM, *]]       = new MutSimSTMMonad[E]
}

private class MutSimTransact[E] extends Transact[MutSim[E, *, *]] {
  def writeTVar[A](tvar: MutSim[E, TVAR, A], value: A): MutSim[E, STM, Unit] =
    MutSTM(SimT.writeTVar(tvar.value)(value))
  def readTVar[A](tvar: MutSim[E, TVAR, A]): MutSim[E, STM, A] = MutSTM(SimT.readTVar(tvar.value))
  def fail[A]: MutSim[E, STM, A]                               = MutSTM(SimT.fail)
  def atomically[A](v: MutSim[E, STM, A]): MutSim[E, IO, A]    = MutIO(SimIO.atomically(v.value))
  def newTVar[A](a: A): MutSim[E, IO, MutSim[E, TVAR, A]]      = MutIO(SimIO.newTVar(a).map(MutVar(_)))
  def panic[A](s: String): MutSim[E, IO, A]                    = MutIO(SimIO.panic(s))
  def cancel(threadId: FiberId): MutSim[E, IO, Unit]           = MutIO(SimIO.cancel(threadId))
  def fiberId: MutSim[E, IO, FiberId]                          = MutIO(SimIO.getFiberId)
}

private class MutSimIOMonad[E] extends StackSafeMonad[MutSim[E, IO, *]] with MonadError[MutSim[E, IO, *], E] {
  def flatMap[A, B](fa: MutSim[E, IO, A])(f: A => MutSim[E, IO, B]): MutSim[E, IO, B] =
    MutIO(fa.value.flatMap(a => f(a).value))
  def pure[A](x: A): MutSim[E, IO, A] =
    MutIO(EitherT.pure(x))
  override def map[A, B](fa: MutSim[E, IO, A])(f: A => B): MutSim[E, IO, B] =
    MutIO(fa.value.map(f))
  def raiseError[A](e: E): MutSim[E, IO, A] =
    MutIO(EitherT.leftT(e))
  def handleErrorWith[A](fa: MutSim[E, IO, A])(f: E => MutSim[E, IO, A]): MutSim[E, IO, A] =
    MutIO(fa.value.recoverWith { case e => f(e).value })
}

private class MutSimSTMMonad[E] extends StackSafeMonad[MutSim[E, STM, *]] {
  def flatMap[A, B](fa: MutSim[E, STM, A])(f: A => MutSim[E, STM, B]): MutSim[E, STM, B] =
    MutSTM(fa.value.flatMap(a => f(a).value))
  def pure[A](x: A): MutSim[E, STM, A] =
    MutSTM(OptionT.pure(x))
  override def map[A, B](fa: MutSim[E, STM, A])(f: A => B): MutSim[E, STM, B] =
    MutSTM(fa.value.map(f))
}
