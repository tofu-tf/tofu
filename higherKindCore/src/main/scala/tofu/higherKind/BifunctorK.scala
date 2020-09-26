package tofu.higherKind

import cats.arrow.FunctionK
import cats.{Functor, ~>}

trait BifunctorK[U[f[_], g[_]]] {

  def bimapK[F[_]: Functor, G[_]: Functor, W[_], Q[_]](ufg: U[F, G])(fw: F ~> W)(gq: G ~> Q): U[W, Q]

  final def leftMapK[F[_]: Functor, G[_]: Functor, W[_]](ufg: U[F, G])(fw: F ~> W): U[W, G] =
    bimapK(ufg)(fw)(FunctionK.id)

  final def rightMapK[F[_]: Functor, G[_]: Functor, Q[_]](ufg: U[F, G])(gq: G ~> Q): U[F, Q] =
    bimapK(ufg)(FunctionK.id)(gq)
}
