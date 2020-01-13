import cats.effect.Bracket
import cats.{ApplicativeError, MonadError}

package object tofu {
  type HasContext[F[_], C] = Context[F] { type Ctx = C }
  object HasContext {
    def apply[F[_], C](implicit hc: HasContext[F, C]): HasContext[F, C] = hc
  }

  type HasLocal[F[_], C] = Local[F] { type Ctx = C }
  object HasLocal {
    def apply[F[_], C](implicit hl: HasLocal[F, C]): HasLocal[F, C] = hl
  }

  type HasContextRun[F[_], G[_], C] = RunContext[F] { type Ctx = C; type Lower[A] = G[A] }
  object HasContextRun {
    def apply[F[_], G[_], C](implicit hcr: HasContextRun[F, G, C]): HasContextRun[F, G, C] = hcr
  }

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]
  type BracketThrow[F[_]]     = Bracket[F, Throwable]

  type TConst[A, B] = A

}
