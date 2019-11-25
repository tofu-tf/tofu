package tofu.concurrent

trait MakeConcurrent[I[_], F[_]]
    extends MakeRef[I, F] with MakeDeferred[I, F] with MakeMVar[I, F] with MakeSemaphore[I, F]
