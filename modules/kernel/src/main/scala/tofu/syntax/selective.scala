package tofu.syntax

import tofu.control.Selective

object selective {
  implicit final class OptionSelectOps[F[_], A](private val fo: F[Option[A]]) extends AnyVal {
    def select(fa: => F[A])(implicit F: Selective[F]): F[A]                   = F.select(fo)(fa)
    def orElses(fo2: => F[Option[A]])(implicit F: Selective[F]): F[Option[A]] = F.orElses(fo)(fo2)
  }

  implicit final class EitherSelectOps[F[_], A, B](private val fe: F[Either[A, B]]) extends AnyVal {
    def selectAp(ff: F[A => B])(implicit F: Selective[F]): F[B] = F.selectAp(fe)(ff)
  }

  implicit final class BooleanSelectOps[F[_]](private val fb: F[Boolean]) extends AnyVal {
    def whens[A](fa: => F[A])(implicit F: Selective[F]): F[Option[A]]   = F.whens(fb)(fa)
    def unlesss[A](fa: => F[A])(implicit F: Selective[F]): F[Option[A]] = F.unlesss(fb)(fa)
    def whens_[A](fa: => F[A])(implicit F: Selective[F]): F[Unit]       = F.whens_(fb)(fa)
    def unlesss_[A](fa: => F[A])(implicit F: Selective[F]): F[Unit]     = F.unlesss_(fb)(fa)
  }

  implicit final class TofuSelectiveOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def selectRight[F1[x] >: F[x]](fo: F[Option[A]])(implicit F: Selective[F1]): F1[A] = F.selectRight(fa, fo)
  }
}
