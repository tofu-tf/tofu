package tofu.higherKind.bi

trait SemigroupalB[U[f[_, _]]] extends FunctorB[U] {
  def map2b[F[_, _], G[_, _], H[_, _]](uf: U[F], ug: U[G])(fk: BiFun2K[F, G, H]): U[H]
}

object SemigroupalB {
  def apply[U[f[_, _]]](implicit su: SemigroupalB[U]): SemigroupalB[U] = su
}
