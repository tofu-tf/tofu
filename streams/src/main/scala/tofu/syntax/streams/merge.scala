package tofu.syntax.streams

import tofu.streams.Merge

private[syntax] final class MergeOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def merge(that: F[A])(implicit ev: Merge[F]): F[A] = ev.merge(fa)(that)
}

private[syntax] trait MergeSyntax {
  implicit def toMergeOps[F[_], A](fa: F[A]): MergeOps[F, A] = new MergeOps(fa)
}
