package tofu.higherKind.bi

trait FunctorB[U[f[_, _]]] {
  def mapb[F[_, _], G[_, _]](uf: U[F])(f: BiFunK[F, G]): U[G]
}

object FunctorB {
  def apply[U[f[_, _]]](implicit fb: FunctorB[U]): FunctorB[U] = fb
}
