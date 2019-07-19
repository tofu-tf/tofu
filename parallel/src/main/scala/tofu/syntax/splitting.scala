package tofu.syntax

import cats.{NonEmptyTraverse, Reducible}
import tofu.parallel.Splitting

object splitting extends Splitting.ToSplittingOps {
  implicit class ParallelSequence1Ops[T[_], F[_], A](val ta: T[F[A]]) extends AnyVal {
    def parSequence1(implicit T: NonEmptyTraverse[T], P: Splitting[F]): F[T[A]] = P.parNonEmptySequence(ta)

    def parSequence1_(implicit T: Reducible[T], P: Splitting[F]): F[Unit] = P.parNonEmptySequence_(ta)
  }

  implicit class ParallelTraverse1Ops[T[_], A](val ta: T[A]) extends AnyVal {
    def parTraverse1[F[_], B](f: A => F[B])(implicit T: NonEmptyTraverse[T], P: Splitting[F]): F[T[B]] =
      P.parNonEmptyTraverse(ta)(f)

    def parTraverse1_[F[_], B](f: A => F[B])(implicit T: Reducible[T], P: Splitting[F]): F[Unit] =
      P.parNonEmptyTraverse_(ta)(f)
  }
}
