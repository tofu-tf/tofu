package tofu.kernel

import tofu.{WithContext, Local, Context, Provide, RunContext}

object types {
  type In[C, F[_]] = WithContext[F, C]

  type HasContext[F[_], C] = Context[F] { type Ctx = C }

  type HasLocal[F[_], C] = Local[F] { type Ctx = C }

  type HasProvide[F[_], G[_], C] = Provide[F] { type Ctx = C; type Lower[A] = G[A] }

  type HasContextRun[F[_], G[_], C] = RunContext[F] { type Lower[A] = G[A]; type Ctx = C; }
}
