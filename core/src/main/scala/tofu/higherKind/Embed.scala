package tofu.higherKind

import cats.FlatMap

trait Embed[T[_[_]]] {
  def embed[F[_]: FlatMap](ft: F[T[F]]): T[F]
}
