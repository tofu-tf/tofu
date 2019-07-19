package tofu.syntax

import cats.{Foldable, Traverse}
import tofu.parallel.Paralleled

object paralleled extends Paralleled.ToParalleledOps {
  implicit class ParallelSequenceOps[T[_], F[_], A](val ta: T[F[A]]) extends AnyVal {
    def parSequence(implicit T: Traverse[T], P: Paralleled[F]): F[T[A]] = P.parSequence(ta)

    def parSequence_(implicit T: Foldable[T], P: Paralleled[F]): F[Unit] = P.parSequence_(ta)
  }

  implicit class ParallelTraverseOps[T[_], A](val ta: T[A]) extends AnyVal {
    def parTraverse[F[_], B](f: A => F[B])(implicit T: Traverse[T], P: Paralleled[F]): F[T[B]] =
      P.parTraverse(ta)(f)

    def parTraverse_[F[_], B](f: A => F[B])(implicit T: Foldable[T], P: Paralleled[F]): F[Unit] =
      P.parTraverse_(ta)(f)
  }
}
