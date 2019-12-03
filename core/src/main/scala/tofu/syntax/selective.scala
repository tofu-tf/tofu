package tofu.syntax

import tofu.control.Selective

object selective extends Selective.ToSelectiveOps {
  implicit final class OptionSelectOps[F[_], A](private val fo: F[Option[A]]) extends AnyVal {
    def select(fa: F[A])(implicit F: Selective[F]): F[A]                   = F.select(fo, fa)
    def orElses(fo2: F[Option[A]])(implicit F: Selective[F]): F[Option[A]] = F.orElses(fo)(fo2)
  }
}
