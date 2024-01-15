package tofu.syntax

import cats.Functor
import cats.effect.kernel.{Deferred, MonadCancel, Ref}
import cats.effect.std.Semaphore
import tofu.lift.Lift
import cats.effect.std.Queue

object lift extends KernelLiftSyntax {
  implicit final class RefLiftSyntax[F[_], A](private val ref: Ref[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G], F: Functor[F]): Ref[G, A] = ref.mapK(lift.liftF)
  }

  implicit final class DeferredLiftSyntax[F[_], A](private val deferred: Deferred[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G]): Deferred[G, A] = deferred.mapK(lift.liftF)
  }

  implicit final class SemaphoreLiftSyntax[F[_]](private val semaphore: Semaphore[F]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G], G: MonadCancel[G, ?]): Semaphore[G] = semaphore.mapK(lift.liftF)
  }

  implicit final class QueueLiftSyntax[F[_], A](private val queue: Queue[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G]): Queue[G, A] = queue.mapK(lift.liftF)
  }
}
