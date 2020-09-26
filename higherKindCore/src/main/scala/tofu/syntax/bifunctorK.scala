package tofu.syntax

import cats.{Functor, ~>}
import tofu.higherKind.BifunctorK

object bifunctorK {

  implicit final class BifunctorKOps[U[f[_], g[_]], F[_], G[_]](private val ufg: U[F, G]) extends AnyVal {
    def bimapK[W[_], Q[_]](fw: F ~> W)(gq: G ~> Q)(implicit BF: BifunctorK[U], F: Functor[F], G: Functor[G]): U[W, Q] =
      BF.bimapK(ufg)(fw)(gq)
    def leftMapK[W[_]](fw: F ~> W)(implicit BF: BifunctorK[U], F: Functor[F], G: Functor[G]): U[W, G]                 =
      BF.leftMapK(ufg)(fw)
    def rightMapK[Q[_]](gq: G ~> Q)(implicit BF: BifunctorK[U], F: Functor[F], G: Functor[G]): U[F, Q]                =
      BF.rightMapK(ufg)(gq)
  }
}
