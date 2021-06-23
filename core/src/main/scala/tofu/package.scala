import cats.effect.Bracket
import cats.ApplicativeError
import cats.MonadError
import cats.data.Ior

package object tofu {
  type BracketThrow[F[_]]           = Bracket[F, Throwable]
  type HasContextRun[F[_], G[_], C] = RunContext[F] { type Lower[A] = G[A]; type Ctx = C; }

  type Execute[F[_]] = ScopedExecute[Scoped.Main, F]

  type Blocks[F[_]]    = Scoped[Scoped.Blocking, F]
  type BlockExec[F[_]] = ScopedExecute[Scoped.Blocking, F]

  type Calculates[F[_]] = Scoped[Scoped.Calculation, F]
  type CalcExec[F[_]]   = ScopedExecute[Scoped.Calculation, F]
  type In[C, F[_]]      = WithContext[F, C]

  type HasContext[F[_], C] = Context.Aux[F, C]

  type HasLocal[F[_], C] = Local.Aux[F, C]

  type HasProvide[F[_], G[_], C] = Provide.Aux[F, G, C]

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]

  type Throws[F[_]]  = Raise[F, Throwable]
  type Catches[F[_]] = Handle[F, Throwable]
  type Tries[F[_]]   = Errors[F, Throwable]

  type TConst[A, B] = A

  type IorC[C[_], E, A] = Ior[C[E], C[A]]
}
