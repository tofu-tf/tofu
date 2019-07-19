package tofu

import cats.effect.{Bracket, ExitCase}

trait Guarantee[F[_]] {
  def bracket[A, B, C](init: F[A])(action: A => F[B])(release: (A, Boolean) => F[C]): F[B]
}

object Guarantee {
  implicit def fromBracket[F[_], E](implicit F: Bracket[F, E]): Guarantee[F] = new Guarantee[F] {
    def bracket[A, B, C](init: F[A])(action: A => F[B])(release: (A, Boolean) => F[C]): F[B] =
      F.bracketCase(init)(action) {
        case (a, ExitCase.Completed) => F.void(release(a, true))
        case (a, _)                  => F.void(release(a, false))
      }
  }
}
