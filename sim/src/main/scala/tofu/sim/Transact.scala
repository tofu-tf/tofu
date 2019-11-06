package tofu.sim

import cats.Monad
import SIM._
import tofu.Raise

sealed trait SIM

object SIM {
  final abstract class TVAR extends SIM
  final abstract class IO   extends SIM
  final abstract class STM  extends SIM

  type IOMonad[F[_ <: SIM, _]]  = Monad[F[IO, *]]
  type STMMonad[F[_ <: SIM, _]] = Monad[F[STM, *]]
  type IORaise[F[_ <: SIM, _], E] = Raise[F[IO, *], E]

  def ioMonad[F[_ <: SIM, _]](implicit F: IOMonad[F]): IOMonad[F]    = F
  def stmMonad[F[_ <: SIM, _]](implicit F: STMMonad[F]): STMMonad[F] = F

  implicit class TVarOps[F[_, _], A](private val tvar: F[TVAR, A]) extends AnyVal {
    def read(implicit t: Transact[F]): F[STM, A]           = t.readTVar(tvar)
    def write(a: A)(implicit t: Transact[F]): F[STM, Unit] = t.writeTVar(tvar, a)
  }
}

trait Transact[F[_, _]] {
  def readTVar[A](tvar: F[TVAR, A]): F[STM, A]
  def writeTVar[A](tvar: F[TVAR, A], value: A): F[STM, Unit]
  def fail[A]: F[STM, A]
  def atomically[A](v: F[STM, A]): F[IO, A]
  def newTVar[A](a: A): F[IO, F[TVAR, A]]
  def panic[A](s: String): F[IO, A]
  def cancel(threadId: FiberId): F[IO, Unit]
  def fiberId: F[IO, FiberId]
}

object Transact {
  def atomically[F[_, _], A](v: F[STM, A])(implicit FA: Transact[F]): F[IO, A] = FA.atomically(v)
  def newTVar[F[_ , _]](implicit at: Transact[F])                               = new NewTVar[F](at)
  def cancel[F[_ , _]](fiberId: FiberId)(implicit at: Transact[F]): F[IO, Unit] = at.cancel(fiberId)
  def fail[F[_ , _], A](implicit at: Transact[F]): F[STM, A] = at.fail
  def panic[F[_ , _], A](s: String)(implicit at: Transact[F]): F[IO, A] = at.panic(s)

  class NewTVar[F[_, _]](private val at: Transact[F]) extends AnyVal {
    def apply[A](a: A): F[IO, F[TVAR, A]] = at.newTVar(a)
  }

}
