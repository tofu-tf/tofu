package tofu.concurrent

import cats.effect.std.Semaphore
import cats.effect.{Async, Sync}

trait MakeSemaphore[I[_], F[_]] {
  def semaphore(count: Long): I[Semaphore[F]]
}

object Semaphores {
  def apply[F[_]](implicit agents: MakeSemaphore[F, F]): MakeSemaphore.Applier[F, F] =
    new MakeSemaphore.Applier[F, F](agents)
}

object MakeSemaphore {
  def apply[I[_], F[_]](implicit mksem: MakeSemaphore[I, F]) = new Applier[I, F](mksem)

  final class Applier[I[_], F[_]](private val mksem: MakeSemaphore[I, F]) extends AnyVal {
    def of(count: Long): I[Semaphore[F]] = mksem.semaphore(count)
  }

  implicit def concurrentSemaphore[I[_]: Sync, F[_]: Async]: MakeSemaphore[I, F] = new MakeSemaphore[I, F] {
    def semaphore(count: Long): I[Semaphore[F]] = Semaphore.in[I, F](count)
  }
}
