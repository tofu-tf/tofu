package tofu.concurrent

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Semaphore

trait MakeSemaphore[I[_], F[_]] {
  def semaphore(count: Long): I[Semaphore[F]]
}

object MakeSemaphore {
  def apply[I[_], F[_]](implicit mksem: MakeSemaphore[I, F]) = new Applier[I, F](mksem)

  class Applier[I[_], F[_]](val mksem: MakeSemaphore[I, F]) extends AnyVal {
    def of(count: Long): I[Semaphore[F]] = mksem.semaphore(count)
  }

  implicit def concurrentSemaphore[I[_]: Sync, F[_]: Concurrent]: MakeSemaphore[I, F] = new MakeSemaphore[I, F] {
    def semaphore(count: Long): I[Semaphore[F]] = Semaphore.in[I, F](count)
  }
}
