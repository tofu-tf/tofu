package tofu.higherKind.bi

trait FunctorBK[U[f[_, _]]] {
  def mapb[F[_, _], G[_, _]](uf: U[F])(f: FunBK[F, G]): U[G]
}

object FunctorBK {
  def apply[U[f[_, _]]](implicit fb: FunctorBK[U]): FunctorBK[U] = fb
}
