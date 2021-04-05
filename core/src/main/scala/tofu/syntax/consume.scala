package tofu.syntax

import cats.{MonoidK, SemigroupK}
import tofu.control.{Optional, Switch}
import tofu.control.ContravariantFilter

object consume extends MonoidK.ToMonoidKOps with SemigroupK.ToSemigroupKOps {
  implicit class TofuConsumeOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def switch[F1[x] >: F[x], B](fb: F1[B])(implicit F: Switch[F1]): F1[Either[A, B]] =
      F.switch(fa, fb)

    def optional(implicit F: Optional[F]): F[Option[A]] = F.optional(fa)

    def contramapFilter[F1[x] >: F[x], B](f: B => Option[A])(implicit F: ContravariantFilter[F1]): F1[B] =
      F.contramapFilter(fa)(f)

    def contraCollect[B](f: PartialFunction[B, A])(implicit F: ContravariantFilter[F]): F[B] =
      F.contraCollect(fa)(f)

    def contraFilter(f: A => Boolean)(implicit F: ContravariantFilter[F]): F[A] =
      F.contraFilter(fa)(f)
  }
}
