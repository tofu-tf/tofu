package tofu.sim

import SIM._
import cats.Monad
import tofu.{Guarantee, Raise}

sealed trait SIM

object SIM extends LLSIM {
  final abstract class TVAR    extends SIM
  final abstract class RUN[+E] extends SIM
  final abstract class STM[+E] extends SIM

  type IOMonad[F[_, _], E]     = Monad[F[RUN[E], *]]
  type VoidMonad[F[_, _]]      = Monad[F[RUN[Nothing], *]]
  type IOGuarantee[F[_, _], E] = Guarantee[F[RUN[E], *]]
  type STMMonad[F[_, _], E]    = Monad[F[STM[E], *]]
  type STMVMonad[F[_, _]]      = Monad[F[STM[Nothing], *]]
  type IORaise[F[_, _], E]     = Raise[F[RUN[E], *], E]

  implicit class TVarOps[F[+_, _], A](private val tvar: F[TVAR, A]) extends AnyVal {
    def read(implicit t: Transact[F]): F[STM[Nothing], A]           = t.readTVar(tvar)
    def write(a: A)(implicit t: Transact[F]): F[STM[Nothing], Unit] = t.writeTVar(tvar, a)
  }

  implicit class STMOps[F[+_, _], E, A](private val stm: F[STM[E], A]) extends AnyVal {
    def atomically(implicit t: Transact[F]): F[RUN[E], A] = t.atomically(stm)
  }
}

trait LLSIM { self: SIM.type =>
  // implicit def stmNothingOps[F[_, _], A](stm: F[STM[Nothing], A]): STMOps[F, Nothing, A] = STMOps[F, Nothing, A](stm)
}

trait Transact[F[_, _]] {
  def readTVar[A](tvar: F[TVAR, A]): F[STM[Nothing], A]
  def writeTVar[A](tvar: F[TVAR, A], value: A): F[STM[Nothing], Unit]
  def fail[A]: F[STM[Nothing], A]
  def atomically[E, A](v: F[STM[E], A]): F[RUN[E], A]
  def newTVar[A](a: A): F[RUN[Nothing], F[TVAR, A]]
  def panic[A](s: String): F[RUN[Nothing], A]
  def cancel(threadId: FiberId): F[RUN[Nothing], Unit]
  def fiberId: F[RUN[Nothing], FiberId]
  def time: F[RUN[Nothing], Long]
  def sleep(nanos: Long): F[RUN[Nothing], Unit]
  def error[E, A](error: E): F[RUN[E], A]
  def attempt[E, A](proc: F[RUN[E], A]): F[RUN[Nothing], Either[E, A]]
  def exec(p: F[RUN[Nothing], Unit]): F[RUN[Nothing], FiberId]
}

object Transact {
  def atomically[F[+_, _], E, A](v: F[STM[E], A])(implicit FA: Transact[F]): F[RUN[E], A]          = FA.atomically(v)
  def newTVar[F[+_, _], E](implicit at: Transact[F])                                               = new NewTVar[F, E](at)
  def cancel[F[+_, _]](fiberId: FiberId)(implicit at: Transact[F]): F[RUN[Nothing], Unit]          = at.cancel(fiberId)
  def fail[F[+_, _], A](implicit at: Transact[F]): F[STM[Nothing], A]                              = at.fail
  def panic[F[+_, _], A](s: String)(implicit at: Transact[F]): F[RUN[Nothing], A]                  = at.panic(s)
  def error[F[+_, _], E, A](err: E)(implicit at: Transact[F]): F[RUN[E], A]                        = at.error(err)
  def exec[F[_, _]](p: F[RUN[Nothing], Unit])(implicit at: Transact[F]): F[RUN[Nothing], FiberId] = at.exec(p)
  def fiberId[F[_, _]](implicit at: Transact[F]): F[RUN[Nothing], FiberId]                        = at.fiberId

  class NewTVar[F[+_, _], E](private val at: Transact[F]) extends AnyVal {
    def of[A](a: A): F[RUN[E], F[TVAR, A]] = at.newTVar(a)
  }

  implicit class SIMPureOps[A](private val a: A) extends AnyVal {
    def pureSTM[F[_, _], E](implicit F: STMMonad[F, E]): F[STM[E], A] = F.pure(a)
    def pureIO[F[_, _], E](implicit F: IOMonad[F, E]): F[RUN[E], A]   = F.pure(a)
  }

  implicit class SIMFIOOps[F[+_, _], E, A](private val fa: F[RUN[E], A]) extends AnyVal {
    def attempt(implicit at: Transact[F]): F[RUN[Nothing], Either[E, A]] = at.attempt[E, A](fa)
  }

  def ioMonad[F[_, _], E](implicit F: IOMonad[F, E]): IOMonad[F, E]              = F
  def iovMonad[F[_, _]](implicit F: IOMonad[F, Nothing]): IOMonad[F, Nothing]    = F
  def stmMonad[F[_, _], E](implicit F: STMMonad[F, E]): STMMonad[F, E]           = F
  def stmvMonad[F[_, _]](implicit F: STMMonad[F, Nothing]): STMMonad[F, Nothing] = F
}
