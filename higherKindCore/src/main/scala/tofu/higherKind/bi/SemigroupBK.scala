package tofu.higherKind.bi

import cats.SemigroupK
import cats.kernel.Semigroup

trait SemigroupBK[F[_, _]] {
  def combinebk[A, B](x: F[A, B], y: F[A, B]): F[A, B]

  def semigroupK[X]: SemigroupK[F[X, *]] = new SemigroupK[F[X, *]] {
    def combineK[A](x: F[X, A], y: F[X, A]): F[X, A] = combinebk(x, y)
  }

  def leftSemigroupK[X]: SemigroupK[F[*, X]] = new SemigroupK[F[*, X]] {
    def combineK[A](x: F[A, X], y: F[A, X]): F[A, X] = combinebk(x, y)
  }

  def semigroup[X, Y]: Semigroup[F[X, Y]] = combinebk(_, _)
}
