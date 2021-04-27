package tofu.syntax.streams

import tofu.streams.ParFlatten

object parFlatten {

  implicit final class ParFlattenOps[F[_], A](private val ffa: F[F[A]]) extends AnyVal {
    def parFlatten(maxConcurrent: Int)(implicit ev: ParFlatten[F]): F[A] = ev.parFlatten(ffa)(maxConcurrent)
    def parFlattenUnbounded(implicit ev: ParFlatten[F]): F[A]            = ev.parFlattenUnbounded(ffa)
  }
}
