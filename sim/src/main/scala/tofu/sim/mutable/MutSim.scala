package tofu.sim
package mutable
import cats.data.{EitherT, OptionT}
import cats.{Monad, MonadError, StackSafeMonad}
import tofu.Guarantee
import tofu.sim.SIM.{RUN, STM, TVAR}
import tofu.sim.mutable.MutSim.{MutIO, MutSTM, MutVar}

sealed trait MutSim[T, A]

object MutSim {
  final case class MutIO[E, A](fa: SimIO[E, A]) extends MutSim[RUN[E], A]
  final case class MutSTM[A](fa: SimT[A])       extends MutSim[STM, A]
  final case class MutVar[A](va: MutTVar[A])    extends MutSim[TVAR, A]

  implicit class IOOPs[E, A](private val sim: MutSim[RUN[E], A]) extends AnyVal {
    def value: SimIO[E, A] = sim match { case MutIO(fa) => fa }
  }
  implicit class STMOps[A](private val sim: MutSim[STM, A]) extends AnyVal {
    def value: SimT[A] = sim match { case MutSTM(fa) => fa }
  }
  implicit class TVarOps[A](private val sim: MutSim[TVAR, A]) extends AnyVal {
    def value: MutTVar[A] = sim match { case MutVar(fa) => fa }
  }

  implicit val transact: Transact[MutSim[*, *]] = new MutSimTransact
  implicit def ioMonad[E]: MonadError[MutSim[RUN[E], *], E] with Guarantee[MutSim[RUN[E], *]] =
    new MutSimIOMonad[E]
  implicit def stmMonad[E]: Monad[MutSim[STM, *]] = new MutSimSTMMonad[E]
}

private class MutSimTransact extends Transact[MutSim[*, *]] {
  def writeTVar[A](tvar: MutSim[TVAR, A], value: A): MutSim[STM, Unit] =
    MutSTM(SimT.writeTVar(tvar.value)(value))
  def readTVar[A](tvar: MutSim[TVAR, A]): MutSim[STM, A]     = MutSTM(SimT.readTVar(tvar.value))
  def fail[A]: MutSim[STM, A]                                = MutSTM(SimT.fail)
  def atomically[E, A](v: MutSim[STM, A]): MutSim[RUN[E], A] = MutIO(SimIO.atomically(v.value))
  def newTVar[E, A](a: A): MutSim[RUN[E], MutSim[TVAR, A]]   = MutIO(SimIO.newTVar(a).map(MutVar(_)))
  def panic[E, A](s: String): MutSim[RUN[E], A]              = MutIO(SimIO.panic(s))
  def cancel[E](threadId: FiberId): MutSim[RUN[E], Unit]     = MutIO(SimIO.cancel(threadId))
  def fiberId[E]: MutSim[RUN[E], FiberId]                    = MutIO(SimIO.getFiberId)
  def time[E]: MutSim[RUN[E], Long]                          = MutIO(SimIO.time)
  def sleep[E](nanos: Long): MutSim[RUN[E], Unit]            = MutIO(SimIO.sleep(nanos))
}

class MutSimIOMonad[E]
    extends StackSafeMonad[MutSim[RUN[E], *]] with MonadError[MutSim[RUN[E], *], E] with Guarantee[MutSim[RUN[E], *]] {
  def flatMap[A, B](fa: MutSim[RUN[E], A])(f: A => MutSim[RUN[E], B]): MutSim[RUN[E], B] =
    MutIO(fa.value.flatMap(a => f(a).value))
  def pure[A](x: A): MutSim[RUN[E], A] = MutIO(EitherT.pure(x))
  override def map[A, B](fa: MutSim[RUN[E], A])(f: A => B): MutSim[RUN[E], B] =
    MutIO(fa.value.map(f))
  def raiseError[A](e: E): MutSim[RUN[E], A] =
    MutIO(EitherT.leftT(e))
  def handleErrorWith[A](fa: MutSim[RUN[E], A])(f: E => MutSim[RUN[E], A]): MutSim[RUN[E], A] =
    MutIO(fa.value.recoverWith { case e => f(e).value })
  def bracket[A, B, C](
      init: MutSim[RUN[E], A]
  )(action: A => MutSim[RUN[E], B])(release: (A, Boolean) => MutSim[RUN[E], C]): MutSim[RUN[E], B] =
    MutIO(SimIO.guarantee(init.value)(a => action(a).value)((a, s) => release(a, s).value))
}

private class MutSimSTMMonad[E] extends StackSafeMonad[MutSim[STM, *]] {
  def flatMap[A, B](fa: MutSim[STM, A])(f: A => MutSim[STM, B]): MutSim[STM, B] =
    MutSTM(fa.value.flatMap(a => f(a).value))
  def pure[A](x: A): MutSim[STM, A] =
    MutSTM(OptionT.pure(x))
  override def map[A, B](fa: MutSim[STM, A])(f: A => B): MutSim[STM, B] =
    MutSTM(fa.value.map(f))
}
