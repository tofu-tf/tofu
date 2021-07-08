package tofu.concurrent

trait MakeConcurrent[I[_], F[_]]
    extends MakeAtom[I, F] with MakeGatekeeper[I, F, Long] with MakeQVar[I, F] with MakeDeferred[I, F]
    with MakeAgent[I, F]

trait MakeAllConcurrent[I[_], F[_]]
    extends MakeRef[I, F] with MakeMVar[I, F] with MakeSemaphore[I, F] with MakeConcurrent[I, F]
