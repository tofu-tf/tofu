package tofu
package kernel

import cats.ApplicativeError
import cats.MonadError
import tofu.concurrent.Exit

object types extends KernelTypes

trait KernelTypes extends Any {
  type In[C, F[_]] = WithContext[F, C]

  @deprecated("Use WithContext instead", "0.10.3")
  type HasContext[F[_], C] = Context[F] { type Ctx = C }

  @deprecated("Use WithLocal instead", "0.10.3")
  type HasLocal[F[_], C] = Local[F] { type Ctx = C }

  @deprecated("Use WithProvide instead", "0.10.3")
  type HasProvide[F[_], G[_], C] = WithProvide[F, G, C]

  @deprecated("Use WithRun instead", "0.10.3")
  type HasContextRun[F[_], G[_], C] = WithRun[F, G, C]

  type AnyK[_] = Any

  type TConst[A, B] = A

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]

  type Throws[F[_]]  = Raise[F, Throwable]
  type Catches[F[_]] = Handle[F, Throwable]
  type Tries[F[_]]   = Errors[F, Throwable]

  type Execute[F[_]] = ScopedExecute[Scoped.Main, F]

  final type Blocks[F[_]] = Scoped[Scoped.Blocking, F]
  type BlockExec[F[_]]    = ScopedExecute[Scoped.Blocking, F]

  type Calculates[F[_]] = Scoped[Scoped.Calculation, F]
  type CalcExec[F[_]]   = ScopedExecute[Scoped.Calculation, F]

  type PerformCont[Ex[_], A]  = Ex[A] => Unit
  type PerformExitCont[E, A]  = Exit[E, A] => Unit
  type PerformOf[F[_], Ex[_]] = PerformVia[F, PerformCont[Ex, *], Unit]
  type Perform[F[_], E]       = PerformOf[F, Exit[E, *]]
  type PerformThrow[F[_]]     = Perform[F, Throwable]
}
