package tofu.higherKind.bi

trait MonoidalBK[U[f[_, _]]] extends SemigroupalBK[U] with PureBK[U] {
  def mapb[F[_, _], G[_, _]](uf: U[F])(f: FunBK[F, G]): U[G] =
    map2b(uf, unitB)(Fun2BK.apply((fa, _) => f(fa)))
}

object MonoidalBK {
  def apply[U[f[_, _]]](implicit u: MonoidalBK[U]): MonoidalBK[U] = u
}
