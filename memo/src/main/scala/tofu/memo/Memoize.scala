package tofu.memo

import simulacrum.typeclass

/** WARNING breaks referential transparency, use with great care */
@typeclass
trait Memoize[F[_]] {
  def memoize[A](fa: F[A]): F[A]

  /** should be redefined if F is at least ApplicativeError */
  def memoizeOnSuccess[A](fa: F[A]): F[A] = memoize(fa)
}

