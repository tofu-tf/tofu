package tofu

import tofu.syntax.Do

class DoSyntaxCheck {
  def bar[F[_]: Do, A, B, C](f: A => F[B], g: B => F[C], h: C => F[Unit], fa: F[A]) =
    for {
      x <- fa
      y <- f(x)
      z <- g(y).flatTap(h)
    } yield (x, y, z)

  def bar2[F[_]: Do, A, B, C, D](f: F[(A, B) => C], g: F[C => D], fa: F[A], fb: F[B]) =
    g <*> f.ap2(fa, fb)

  def eitherCheck[A, B, C, E, E1 <: E, E2 <: E, E3 <: E](
      fa: Either[E1, A],
      f: A => Either[E2, B],
      g: B => Either[E3, C],
  ) = for {
    x <- fa
    y <- f(x)
    z <- g(y)
  } yield (x, y, z)
}
