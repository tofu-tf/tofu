package tofu.kernel

import tofu.{WithContext, Local, Context}
import tofu.WithProvide
import tofu.WithRun

object types {
  type In[C, F[_]] = WithContext[F, C]

  type HasContext[F[_], C] = Context[F] { type Ctx = C }

  type HasLocal[F[_], C] = Local[F] { type Ctx = C }

  type HasProvide[F[_], G[_], C] = WithProvide[F, G, C]

  type HasContextRun[F[_], G[_], C] = WithRun[F, G, C]

  type AnyK[_] = Any
}
