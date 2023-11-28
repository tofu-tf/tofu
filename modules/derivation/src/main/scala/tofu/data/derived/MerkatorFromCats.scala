package tofu.data.derived
import cats.Monad
import magnolia1.Monadic

class MerkatorFromCats[F[_]](implicit F: Monad[F]) extends Monadic[F] {
  def point[A](value: A): F[A]                       = F.pure(value)
  def flatMap[A, B](from: F[A])(fn: A => F[B]): F[B] = F.flatMap(from)(fn)
  def map[A, B](from: F[A])(fn: A => B): F[B]        = F.map(from)(fn)
}
