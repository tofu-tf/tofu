package tofu.higherKind

import cats.FlatMap
import simulacrum.typeclass

@typeclass trait Embed[U[_[_]]] {
  def embed[F[_]: FlatMap](ft: F[U[F]]): U[F]
}

object Embed {
  def of[U[_[_]], F[_]: FlatMap](fuf: F[U[F]])(implicit embed: Embed[U]): U[F] = embed.embed(fuf)
}
