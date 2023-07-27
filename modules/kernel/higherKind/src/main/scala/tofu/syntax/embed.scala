package tofu.syntax

import cats.FlatMap
import tofu.higherKind.Embed

object embed {
  implicit final class TofuEmbedOps[F[_], T[_[_]]](private val underlying: F[T[F]]) extends AnyVal {
    def embed(implicit embed: Embed[T], F: FlatMap[F]): T[F] = embed.embed(underlying)
  }
}
