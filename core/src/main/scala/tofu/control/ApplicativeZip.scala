package tofu.control
import cats.{Applicative, Apply}

/** mix-in for Apply instances via `map2` */
trait ApplyZip[F[_]] extends Apply[F] {
  def zipWith[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = zipWith(ff, fa)(_ apply _)
}

/** mix-in for Applicative instances via `map2` */
trait ApplicativeZip[F[_]] extends ApplyZip[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] = zipWith(fa, unit)((a, _) => f(a))
}
