package tofu.higherKind.bi

trait SemigroupalBK[U[f[_, _]]] extends FunctorBK[U] {
  def map2b[F[_, _], G[_, _], H[_, _]](uf: U[F], ug: U[G])(fk: Fun2BK[F, G, H]): U[H]
}

object SemigroupalBK {
  def apply[U[f[_, _]]](implicit su: SemigroupalBK[U]): SemigroupalBK[U] = su
}
