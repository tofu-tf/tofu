package tofu.syntax.streams

import tofu.streams.ParFlatten

private[syntax] final class ParFlattenOps[F[_], A](private val ffa: F[F[A]]) extends AnyVal {
  def parFlatten(maxConcurrent: Int)(implicit ev: ParFlatten[F]): F[A] = ev.parFlatten(ffa)(maxConcurrent)
  def parFlattenUnbounded(implicit ev: ParFlatten[F]): F[A]            = ev.parFlattenUnbounded(ffa)
}

private[syntax] trait ParFlattenSyntax {
  implicit def toParFlattenOps[F[_], A](ffa: F[F[A]]): ParFlattenOps[F, A] = new ParFlattenOps(ffa)
}
