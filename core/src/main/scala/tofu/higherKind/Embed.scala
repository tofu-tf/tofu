package tofu.higherKind

import cats.FlatMap
import simulacrum.typeclass

@typeclass trait Embed[U[_[_]]] {
  def embed[F[_]: FlatMap](ft: F[U[F]]): U[F]
}
