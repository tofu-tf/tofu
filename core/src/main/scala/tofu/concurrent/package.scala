package tofu

package object concurrent {
  type Refs[F[_]]       = MakeRef[F, F]
  type Semaphores[F[_]] = MakeSemaphore[F, F]
  type MVars[F[_]]      = MakeMVar[F, F]
  type Deferreds[F[_]]  = MakeDeferred[F, F]
}
