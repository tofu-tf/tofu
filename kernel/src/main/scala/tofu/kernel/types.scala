package tofu
package kernel

import cats.ApplicativeError
import cats.MonadError

object types extends KernelTypes

trait KernelTypes extends Any {
  type In[C, F[_]] = WithContext[F, C]

  type HasContext[F[_], C] = Context[F] { type Ctx = C }

  type HasLocal[F[_], C] = Local[F] { type Ctx = C }

  type HasProvide[F[_], G[_], C] = WithProvide[F, G, C]

  type HasContextRun[F[_], G[_], C] = WithRun[F, G, C]

  type AnyK[_] = Any

  type TConst[A, B] = A

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]

  type Throws[F[_]]  = Raise[F, Throwable]
  type Catches[F[_]] = Handle[F, Throwable]
  type Tries[F[_]]   = Errors[F, Throwable]
}
