package tofu.syntax

import cats.FlatMap
import tofu.higherKind.Embed
import tofu.higherKind.Embed

object embed {
  final implicit class FPutilsEmbedOps[F[_], T[_[_]]](private val underlying: F[T[F]]) extends AnyVal {
    def embed(implicit embed: Embed[T], F: FlatMap[F]): T[F] = embed.embed(underlying)
  }
}
