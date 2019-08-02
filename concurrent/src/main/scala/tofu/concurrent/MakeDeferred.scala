package tofu.concurrent

import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Sync}

trait MakeDeferred[I[_], F[_]] {
  def deferred[A]: I[Deferred[F, A]]
}

object MakeDeferred {
  def apply[I[_], F[_], A](implicit make: MakeDeferred[I, F]): I[Deferred[F, A]] = make.deferred[A]

  implicit def concurrentMakeDeferred[I[_]: Sync, F[_]: Concurrent]: MakeDeferred[I, F] = new MakeDeferred[I, F] {
    def deferred[A]: I[Deferred[F, A]] = Deferred.in[I, F, A]
  }
}
