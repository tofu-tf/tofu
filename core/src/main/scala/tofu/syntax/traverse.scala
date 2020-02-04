package tofu.syntax

import cats.{Applicative, Traverse, TraverseFilter}
import cats.syntax.functor._

object traverse {
  implicit final class TraverseOps[F[_], A](private val ta: F[A]) extends AnyVal {
    def traverseKey[G[_]: Applicative, B](f: A => G[B])(implicit T: Traverse[F]): G[F[(A, B)]] =
      T.traverse(ta)(a => f(a).map((a, _)))

    def traverseKeyFilter[G[_]: Applicative, B](f: A => G[Option[B]])(implicit TF: TraverseFilter[F]): G[F[(A, B)]] =
      TF.traverseFilter(ta)(a => f(a).map(_.map((a, _))))
  }
}
