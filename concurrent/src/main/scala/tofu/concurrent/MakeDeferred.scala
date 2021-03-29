package tofu.concurrent

import cats.effect.concurrent.TryableDeferred
import cats.effect.{Concurrent, Sync}
import cats.effect.Deferred

trait MakeDeferred[I[_], F[_]] {
  def deferred[A]: I[Deferred[F, A]]
}

trait TryableDeferreds[F[_]] extends MakeDeferred[F, F] {
  def tryable[A]: F[TryableDeferred[F, A]]
}

object Deferreds {
  def apply[F[_], A](implicit make: Deferreds[F]): F[Deferred[F, A]] = make.deferred[A]
}

object MakeDeferred extends PolymorphicMakeDefferedInstance {
  def apply[I[_], F[_], A](implicit make: MakeDeferred[I, F]): I[Deferred[F, A]] = make.deferred[A]

  def tryable[F[_], A](implicit make: TryableDeferreds[F]): F[TryableDeferred[F, A]] = make.tryable[A]

  implicit def concurrentTryableDeferreds[F[_]: Concurrent]: TryableDeferreds[F] = new TryableDeferreds[F] {
    def deferred[A]: F[Deferred[F, A]]       = Deferred[F, A]
    def tryable[A]: F[TryableDeferred[F, A]] = Deferred.tryable[F, A]
  }
}
trait PolymorphicMakeDefferedInstance {
  implicit def concurrentMakeDeferred[I[_]: Sync, F[_]: Concurrent]: MakeDeferred[I, F] = new MakeDeferred[I, F] {
    def deferred[A]: I[Deferred[F, A]] = Deferred.in[I, F, A]
  }
}
