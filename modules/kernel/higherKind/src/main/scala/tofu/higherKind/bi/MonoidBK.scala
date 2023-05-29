package tofu.higherKind.bi

import cats.MonoidK
import cats.kernel.Monoid
trait MonoidBK[F[_, _]] extends SemigroupBK[F] {
  def emptybk[A, B]: F[A, B]

  def monoidK[X]: MonoidK[F[X, _]] = new MonoidK[F[X, _]] {
    def combineK[A](x: F[X, A], y: F[X, A]): F[X, A] = combinebk(x, y)

    def empty[A]: F[X, A] = emptybk
  }

  def leftMonoidK[X, Y]: MonoidK[F[_, X]] = new MonoidK[F[_, X]] {
    def combineK[A](x: F[A, X], y: F[A, X]): F[A, X] = combinebk(x, y)

    def empty[A]: F[A, X] = emptybk
  }

  def monoid[X, Y]: Monoid[F[X, Y]] = new Monoid[F[X, Y]] {
    def combine(x: F[X, Y], y: F[X, Y]): F[X, Y] = combinebk(x, y)

    def empty: F[X, Y] = emptybk
  }
}
