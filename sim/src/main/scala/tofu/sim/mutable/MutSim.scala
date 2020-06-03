package tofu.sim
package mutable

import scala.annotation.unchecked.uncheckedVariance

import cats.data.EitherT
import cats.effect.{Bracket, ExitCase}
import cats.{Monad, StackSafeMonad}
import tofu.sim.SIM._
import tofu.sim.mutable.MutSim.{MutIO, MutSTM, MutVar}
import tofu.HandleTo

sealed trait MutSim[+T, A]

object MutSim {
  final case class MutIO[+E, A](fa: SimIO[E @uncheckedVariance, A]) extends MutSim[RUN[E], A]
  final case class MutSTM[+E, A](fa: SimT[E, A])                    extends MutSim[STM[E], A]
  final case class MutVar[A](va: MutTVar[A])                        extends MutSim[TVAR, A]

  implicit class IOOPs[E, A](private val sim: MutSim[RUN[E], A])  extends AnyVal {
    def value: SimIO[E, A] = sim match { case MutIO(fa) => fa }
  }
  implicit class STMOps[E, A](private val sim: MutSim[STM[E], A]) extends AnyVal {
    def value: SimT[E, A] = sim match { case MutSTM(fa) => fa }
  }
  implicit class TVarOps[A](private val sim: MutSim[TVAR, A])     extends AnyVal {
    def value: MutTVar[A] = sim match { case MutVar(fa) => fa }
  }

  implicit val transact: Transact[MutSim[*, *]]      = new MutSimTransact
  implicit def ioMonad[E]: MutSimIOMonad[E]          = new MutSimIOMonad[E]
  implicit def stmMonad[E]: Monad[MutSim[STM[E], *]] = new MutSimSTMMonad[E]
}

private class MutSimTransact extends Transact[MutSim[*, *]] {
  def writeTVar[A](tvar: MutSim[TVAR, A], value: A): MutSim[STM[Nothing], Unit]  =
    MutSTM(SimT.writeTVar(tvar.value)(value))
  def readTVar[A](tvar: MutSim[TVAR, A]): MutSim[STM[Nothing], A]                = MutSTM(SimT.readTVar(tvar.value))
  def fail[A]: MutSim[STM[Nothing], A]                                           = MutSTM(SimT.fail)
  def atomically[E, A](v: MutSim[STM[E], A]): MutSim[RUN[E], A]                  = MutIO(SimIO.atomically(v.value))
  def newTVar[A](a: A): MutSim[RUN[Nothing], MutSim[TVAR, A]]                    = MutIO(SimIO.newTVar(a).map(MutVar(_)))
  def panic[A](s: String): MutSim[RUN[Nothing], A]                               = MutIO(SimIO.panic(s))
  def cancel(threadId: FiberId): MutSim[RUN[Nothing], Unit]                      = MutIO(SimIO.cancel(threadId))
  def fiberId: MutSim[RUN[Nothing], FiberId]                                     = MutIO(SimIO.getFiberId)
  def time: MutSim[RUN[Nothing], Long]                                           = MutIO(SimIO.time)
  def sleep(nanos: Long): MutSim[RUN[Nothing], Unit]                             = MutIO(SimIO.sleep(nanos))
  def error[E, A](err: E): MutSim[RUN[E], A]                                     = MutIO(SimIO.raise(err))
  def attempt[E, A](proc: MutSim[RUN[E], A]): MutSim[RUN[Nothing], Either[E, A]] = MutIO(SimIO.attempt(proc.value))
  def exec(p: MutSim[RUN[Nothing], Unit]): MutSim[RUN[Nothing], FiberId]         = MutIO(SimIO.exec(p.value))
}

class MutSimIOMonad[E]
    extends StackSafeMonad[MutSim[RUN[E], *]] with Bracket[MutSim[RUN[E], *], E]
    with HandleTo[MutSim[RUN[E], *], MutSim[RUN[Nothing], *], E] {

  override def lift[A](fa: MutSim[RUN[Nothing], A]): MutSim[RUN[E], A] = fa match {
    case MutIO(fa) => MutIO[E, A](fa.leftMap(x => x))
  }

  def flatMap[A, B](fa: MutSim[RUN[E], A])(f: A => MutSim[RUN[E], B]): MutSim[RUN[E], B]      =
    MutIO(fa.value.flatMap(a => f(a).value))
  def pure[A](x: A): MutSim[RUN[E], A]                                                        = MutIO(EitherT.pure(x))
  override def map[A, B](fa: MutSim[RUN[E], A])(f: A => B): MutSim[RUN[E], B]                 =
    MutIO(fa.value.map(f))
  def raiseError[A](e: E): MutSim[RUN[E], A]                                                  =
    MutIO(EitherT.leftT(e))
  def handleErrorWith[A](fa: MutSim[RUN[E], A])(f: E => MutSim[RUN[E], A]): MutSim[RUN[E], A] =
    MutIO(fa.value.recoverWith { case e => f(e).value })
  def bracketCase[A, B](acquire: MutSim[RUN[E], A])(use: A => MutSim[RUN[E], B])(
      release: (A, ExitCase[E]) => MutSim[RUN[E], Unit]
  ): MutSim[RUN[E], B]                                                                        =
    MutIO(SimIO.bracket(acquire.value)(a => use(a).value)((a, ec) => release(a, ec).value))

  def handleWith[A](fa: MutSim[RUN[E], A])(f: E => MutSim[RUN[Nothing], A]): MutSim[RUN[Nothing], A] =
    MutIO[Nothing, A](SimIO.attempt[Nothing, E, A](fa.value).flatMap[Nothing, A] {
      case Right(x) => SimIO.pure(x)
      case Left(e)  => f(e).value
    })
  def restore[A](fa: MutSim[RUN[E], A]): MutSim[RUN[Nothing], Option[A]]                             =
    MutIO[Nothing, Option[A]](SimIO.attempt[Nothing, E, A](fa.value).flatMap[Nothing, Option[A]] {
      case Right(x) => SimIO.pure(Some(x))
      case Left(_)  => SimIO.pure(None)
    })
}

private class MutSimSTMMonad[E] extends StackSafeMonad[MutSim[STM[E], *]] {
  def flatMap[A, B](fa: MutSim[STM[E], A])(f: A => MutSim[STM[E], B]): MutSim[STM[E], B] =
    MutSTM(fa.value.flatMap(a => f(a).value))
  def pure[A](x: A): MutSim[STM[E], A]                                                   =
    MutSTM[E, A](EitherT.pure(x))
  override def map[A, B](fa: MutSim[STM[E], A])(f: A => B): MutSim[STM[E], B]            =
    MutSTM(fa.value.map(f))
}
