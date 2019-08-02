package tofu

package object concurrent {
  @deprecated("use `concurrent.syntax.traverse`", since = "0.1.1")
  val ConcurrentOps = tofu.concurrent.syntax.traverse

  type Refs[F[_]]       = MakeRef[F, F]
  type Semaphores[F[_]] = MakeSemaphore[F, F]
  type MVars[F[_]]      = MakeMVar[F, F]
  type Deferreds[F[_]]  = MakeDeferred[F, F]
}
