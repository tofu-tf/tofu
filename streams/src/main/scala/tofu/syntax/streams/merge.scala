package tofu.syntax.streams

import tofu.streams.Merge

object merge {

  implicit final class MergeOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def merge(that: F[A])(implicit ev: Merge[F]): F[A] = ev.merge(fa)(that)
  }
}
