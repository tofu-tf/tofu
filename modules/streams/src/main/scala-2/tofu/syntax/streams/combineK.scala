package tofu.syntax.streams

import cats.syntax.semigroupk._
import cats.{Defer, SemigroupK}

private[syntax] final class CombineKOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def repeat(implicit F: SemigroupK[F], D: Defer[F]): F[A] = fa <+> D.defer(repeat)
}

private[syntax] trait CombineKSyntax {
  implicit def toCombineKOps[F[_], A](fa: F[A]): CombineKOps[F, A] = new CombineKOps(fa)
}
