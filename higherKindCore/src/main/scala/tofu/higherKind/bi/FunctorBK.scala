package tofu.higherKind.bi

trait FunctorBK[U[f[_, _]]] {
  def mapb[F[_, _], G[_, _]](uf: U[F])(f: FunBK[F, G]): U[G]

  def widen[F[_, _], F1[x, y] >: F[x, y]](uf: U[F]): U[F1] = uf.asInstanceOf[U[F1]]
}

object FunctorBK {
  def apply[U[f[_, _]]](implicit fb: FunctorBK[U]): FunctorBK[U] = fb
}
