package tofu

package object concurrent {
  @deprecated("use `concurrent.syntax.traverse`", since = "0.1.1")
  val ConcurrentOps = tofu.concurrent.syntax.traverse

  type Daemon0[F[_]] = Daemon[F, Throwable, Unit]

  type Refs[F[_]]       = MakeRef[F, F]
  type Semaphores[F[_]] = MakeSemaphore[F, F]
  type MVars[F[_]]      = MakeMVar[F, F]
  type Deferreds[F[_]]  = MakeDeferred[F, F]

  def newRef[F[_]: Refs]              = MakeRef[F, F]
  def newSemaphore[F[_]: Semaphores]  = MakeSemaphore[F, F]
  def newVar[F[_]: MVars]             = MakeMVar[F, F]
  def newDeffered[F[_]: Deferreds, A] = MakeDeferred[F, F, A]
}
