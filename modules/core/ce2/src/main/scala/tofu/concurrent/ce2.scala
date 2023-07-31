package tofu.concurrent

object ce2 {
  type Refs[F[_]]       = MakeRef[F, F]
  type Semaphores[F[_]] = MakeSemaphore[F, F]

  def newRef[F[_]: Refs]             = MakeRef[F, F]
  def newSemaphore[F[_]: Semaphores] = MakeSemaphore[F, F]
}
