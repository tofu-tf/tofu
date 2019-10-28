package tofu.sim

import cats.Monad

trait Transact[F[_], TVar[_]] {
  def readTVar[A](tvar: TVar[A]): F[A]
  def writeTVar[A](tvar: TVar[A], value: A): F[Unit]
  def fail[A]: F[A]
}

trait Atomic[F[_]] {
  type T[_]
  type TVar[_]
  implicit def transact: Transact[T, TVar]
  implicit def tmonad: Monad[T]

  def atomically[A](v: T[A]): F[A]
  def newTVal[A](a: A): F[TVar[A]]
}

trait Simulated[F[_]] {
  type FiberId
  def panic[A](s: String): F[A]
  def cancel(threadId: FiberId): F[Unit]
  def fiberId: F[FiberId]
}
