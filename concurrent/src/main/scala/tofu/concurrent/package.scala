package tofu

package object concurrent {
  @deprecated("use `concurrent.syntax.traverse`", since = "0.1.1")
  val ConcurrentOps = tofu.concurrent.syntax.traverse

  type MakeStoplight[I[_], F[_]] = MakeGatekeeper[I, F, Long]
  type MakeMutex[I[_], F[_]]     = MakeGatekeeper[I, F, Boolean]

  type Stoplight[F[_]] = Gatekeeper[F, Long]
  type Mutex[F[_]]     = Gatekeeper[F, Boolean]

  type Daemon0[F[_]]    = Daemon[F, Throwable, Unit]
  type Refs[F[_]]       = MakeRef[F, F]
  type Semaphores[F[_]] = MakeSemaphore[F, F]
  type MVars[F[_]]      = MakeMVar[F, F]
  type Deferreds[F[_]]  = MakeDeferred[F, F]

  type Atoms[F[_]]      = MakeAtom[F, F]
  type QVars[F[_]]      = MakeQVar[F, F]
  type Stoplights[F[_]] = MakeStoplight[F, F]
  type Mutexes[F[_]]    = MakeMutex[F, F]
  type Agents[F[_]]     = MakeAgent[F, F]

  def newRef[F[_]: Refs]              = MakeRef[F, F]
  def newSemaphore[F[_]: Semaphores]  = MakeSemaphore[F, F]
  def newVar[F[_]: MVars]             = MakeMVar[F, F]
  def newDeffered[F[_]: Deferreds, A] = MakeDeferred[F, F, A]

  def newAtom[F[_]: Atoms] = MakeAtom[F, F]
  def newQVar[F[_]: QVars] = MakeQVar[F, F]
  def newGatekeeper[F[_]]  = MakeGatekeeper[F, F]

  type DaemonThrow[F[_], A] = Daemon[F, Throwable, A]
  type DaemonTask[F[_]]     = DaemonThrow[F, Unit]

  type DaemonicThrow[F[_]] = Daemonic[F, Throwable]
}
