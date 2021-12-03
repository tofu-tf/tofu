package tofu.syntax

import tofu.higherKind.bi.{Fun2BK, FunBK, FunctorBK, SemigroupalBK}

object functorbk {
  implicit class TofuFunctorBKOps[U[f[_, _]], F[_, _]](private val uf: U[F]) extends AnyVal {
    def mapb[G[_, _]](f: F FunBK G)(implicit U: FunctorBK[U]): U[G]                               = U.mapb(uf)(f)
    def map2b[G[_, _], H[_, _]](ug: U[G])(f: Fun2BK[F, G, H])(implicit U: SemigroupalBK[U]): U[H] = U.map2b(uf, ug)(f)
    def widenb[F1[x, y] >: F[x, y]](implicit U: FunctorBK[U]): U[F1]                              = U.widen[F, F1](uf)
  }
}
