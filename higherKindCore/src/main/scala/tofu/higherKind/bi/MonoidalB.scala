package tofu.higherKind.bi

import simulacrum.typeclass
import cats.Semigroupal

trait MonoidalB[U[f[_, _]]] extends SemigroupalB[U] with PureB[U] {
  def mapb[F[_, _], G[_, _]](uf: U[F])(f: BiFunK[F, G]): U[G] =
    map2b(uf, unitB)(BiFun2K.apply((fa, _) => f(fa)))
}

object MonoidalB {
  def apply[U[f[_, _]]](implicit u: MonoidalB[U]): MonoidalB[U] = u
}
