package tofu.sim

import cats.Monad
import SIM._
import cats.effect.Fiber
import tofu.{Guarantee, Raise}

sealed trait SIM

object SIM {
  final abstract class TVAR    extends SIM
  final abstract class RUN[+E] extends SIM
  final abstract class STM     extends SIM

  type IOMonad[F[_, _], E]     = Monad[F[RUN[E], *]]
  type IOGuarantee[F[_, _], E] = Guarantee[F[RUN[E], *]]
  type STMMonad[F[_, _]]       = Monad[F[STM, *]]
  type IORaise[F[_, _], E]     = Raise[F[RUN[E], *], E]

  def ioMonad[F[_, _], E](implicit F: IOMonad[F, E]): IOMonad[F, E] = F
  def stmMonad[F[_, _]](implicit F: STMMonad[F]): STMMonad[F]       = F

  implicit class TVarOps[F[_, _], A](private val tvar: F[TVAR, A]) extends AnyVal {
    def read(implicit t: Transact[F]): F[STM, A]           = t.readTVar(tvar)
    def write(a: A)(implicit t: Transact[F]): F[STM, Unit] = t.writeTVar(tvar, a)
  }

  implicit class STMOps[F[_, _], A](private val stm: F[STM, A]) extends AnyVal {
    def atomically[E](implicit t: Transact[F]): F[RUN[E], A] = t.atomically(stm)
  }
}

trait Transact[F[_, _]] {
  def readTVar[A](tvar: F[TVAR, A]): F[STM, A]
  def writeTVar[A](tvar: F[TVAR, A], value: A): F[STM, Unit]
  def fail[A]: F[STM, A]
  def atomically[E, A](v: F[STM, A]): F[RUN[E], A]
  def newTVar[E, A](a: A): F[RUN[E], F[TVAR, A]]
  def panic[E, A](s: String): F[RUN[E], A]
  def cancel[E](threadId: FiberId): F[RUN[E], Unit]
  def fiberId[E]: F[RUN[E], FiberId]
  def time[E]: F[RUN[E], Long]
  def sleep[E](nanos: Long): F[RUN[E], Unit]
//  def fork[E, A](f: F[RUN[E], A]): F[RUN[E], Fiber[F[RUN[E], *], A]]
}

object Transact {
  def atomically[F[_, _], E, A](v: F[STM, A])(implicit FA: Transact[F]): F[RUN[E], A] = FA.atomically(v)
  def newTVar[F[_, _], E](implicit at: Transact[F])                                   = new NewTVar[F, E](at)
  def cancel[F[_, _], E](fiberId: FiberId)(implicit at: Transact[F]): F[RUN[E], Unit] = at.cancel(fiberId)
  def fail[F[_, _], A](implicit at: Transact[F]): F[STM, A]                           = at.fail
  def panic[F[_, _], E, A](s: String)(implicit at: Transact[F]): F[RUN[E], A]         = at.panic(s)

  class NewTVar[F[_, _], E](private val at: Transact[F]) extends AnyVal {
    def apply[A](a: A): F[RUN[E], F[TVAR, A]] = at.newTVar(a)
  }

  implicit class SIMPureOps[A](private val a: A) extends AnyVal {
    def pureSTM[F[_, _]](implicit F: STMMonad[F]): F[STM, A]        = F.pure(a)
    def pureIO[F[_, _], E](implicit F: IOMonad[F, E]): F[RUN[E], A] = F.pure(a)
  }
}
