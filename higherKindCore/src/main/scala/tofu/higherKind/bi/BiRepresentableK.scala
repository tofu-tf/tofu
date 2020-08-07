package tofu.higherKind.bi

import tofu.control.Bind
import tofu.higherKind.bi.RepresentableB.Tab

trait RepresentableB[U[f[_, _]]] extends MonoidalBK[U] with EmbedBK[U] {
  def bitabulate[F[_, _]](repr: RepBK[U, *, *] FunBK F): U[F]

  final def tab[F[_, _]]: Tab[U, F] = new Tab(this)

  def biembed[F[_, _]](fu: F[U[F], U[F]])(implicit F: Bind[F]): U[F] =
    tab(repr => F.foldWithC(fu)(repr(_))(repr(_)))

  def map2b[F[_, _], G[_, _], H[_, _]](uf: U[F], ug: U[G])(fk: Fun2BK[F, G, H]): U[H] =
    tab(repr => fk(repr(uf), repr(ug)))

  def pureB[F[_, _]](point: BiPoint[F]): U[F] = tab[F](_ => point.apply)
}

object RepresentableB {
  def apply[U[f[_, _]]](implicit u: RepresentableB[U]): RepresentableB[U] = u

  class Tab[U[f[_, _]], F[_, _]](private val repr: RepresentableB[U]) extends AnyVal {
    type E1
    type A1
    def apply(maker: FunBK.Maker[RepBK[U, *, *], F, E1, A1]): U[F] = repr.bitabulate(maker)
  }
}
