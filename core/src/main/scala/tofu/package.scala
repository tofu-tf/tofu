import cats.effect.Bracket
import cats.{ApplicativeError, MonadError}

package object tofu {
  type HasContext[F[_], C] = Context[F] { type Ctx = C }

  type HasLocal[F[_], C] = Local[F] { type Ctx = C }

  type HasProvide[F[_], G[_], C] = Provide[F] { type Ctx = C; type Lower[A] = G[A] }

  type HasContextRun[F[_], G[_], C] = RunContext[F] { type Ctx = C; type Lower[A] = G[A] }

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]
  type BracketThrow[F[_]]     = Bracket[F, Throwable]

  type TConst[A, B] = A

  private[tofu] type AnyK[_] = Any
}
