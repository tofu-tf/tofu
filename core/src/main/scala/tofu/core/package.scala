package tofu
import cats.effect.Bracket

package object core {
  type BracketThrow[F[_]]           = Bracket[F, Throwable]
  type HasContextRun[F[_], G[_], C] = RunContext[F] { type Lower[A] = G[A]; type Ctx = C; }

  type Execute[F[_]] = ScopedExecute[Scoped.Main, F]

  type Blocks[F[_]]    = Scoped[Scoped.Blocking, F]
  type BlockExec[F[_]] = ScopedExecute[Scoped.Blocking, F]

  type Calculates[F[_]] = Scoped[Scoped.Calculation, F]
  type CalcExec[F[_]]   = ScopedExecute[Scoped.Calculation, F]
}
