import cats.effect.Bracket
import cats.data.Ior
import tofu.kernel.KernelTypes

package object tofu extends KernelTypes {

  type BracketThrow[F[_]] = Bracket[F, Throwable]

  type IorC[C[_], E, A] = Ior[C[E], C[A]]

  type Execute[F[_]] = ScopedExecute[Scoped.Main, F]

  type Blocks[F[_]]    = Scoped[Scoped.Blocking, F]
  type BlockExec[F[_]] = ScopedExecute[Scoped.Blocking, F]

  type Calculates[F[_]] = Scoped[Scoped.Calculation, F]
  type CalcExec[F[_]]   = ScopedExecute[Scoped.Calculation, F]
}
