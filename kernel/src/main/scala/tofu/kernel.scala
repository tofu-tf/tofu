package tofu
import cats.ApplicativeError
import cats.data.Ior
import cats.MonadError

object kernel extends KernelTypes

trait KernelTypes {
  type In[C, F[_]] = WithContext[F, C]

  type HasContext[F[_], C] = Context[F] { type Ctx = C }

  type HasLocal[F[_], C] = Local[F] { type Ctx = C }

  type HasProvide[F[_], G[_], C] = Provide[F] { type Ctx = C; type Lower[A] = G[A] }

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]

  type Throws[F[_]]  = Raise[F, Throwable]
  type Catches[F[_]] = Handle[F, Throwable]
  type Tries[F[_]]   = Errors[F, Throwable]

  type TConst[A, B] = A

  private[tofu] type AnyK[_] = Any

  type IorC[C[_], E, A] = Ior[C[E], C[A]]

}
