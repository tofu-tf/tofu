package examples

import cats.data.Chain
import tofu.alias.Do

class DoMonadExamples {
  def bar[F[_]: Do, A, B, C](f: A => F[B], g: B => F[C], h: C => F[Unit], fa: F[A]) =
    for {
      x <- fa
      y <- f(x)
      z <- g(y).flatTap(h)
    } yield (x, y, z)

  def bar2[F[_]: Do, A, B, C, D](f: F[(A, B) => C], g: F[C => D], fa: F[A], fb: F[B]) =
    g <*> f.ap2(fa, fb)

  def covCheck[A, B, C, E, F[_], F1[x] >: F[x], F2[x] >: F1[x]: Do](
      fa: F[A],
      f: A => F1[B],
      g: B => F2[C],
  ) = for {
    x <- fa
    y <- f(x)
    z <- g(y)
  } yield (x, y, z)

  def eitherCheck2[A, B <: A, C <: A, E, E1 <: E, E2 <: E, E3 <: E](
      fb: Either[E1, Boolean],
      fx: Either[E2, B],
      fy: Either[E3, C],
  ) = {
    val res                    = fb.ifM(fx, fy)
    val resCheck: Either[E, A] = res
    resCheck
  }

  def doStaticSyntaxCheck[F[_]: Do, A](a: A): F[A] = {
    Do[F].unit
    Do[F].pure(a)
  }

  def doLazySyntaxCheck2[F[_]: Do, A](fa: => F[A], fb: F[Boolean]) = {
    Do ~ fa when 1 == 2
    Do ~ fa unlessOpt 1 == 2
    Do ~ fa whenM fb
    Do ~ fa unlessOptM fb
  }

  def doLoopSyntaxCheck = {
    Do loop 1 iterate (x => List(x * 2, x * 3)) whileM (_ < 100)
    Do loop 1 iterate (x => List(x * 2, x * 3)) untilM (_ > 100)
    Do loop 27 tailRecM (x =>
      (
        Chain.one(x),
        if (x % 2 == 0) Left(x / 2)
        else if (x > 1) Left(x * 3 + 1)
        else Right(())
      )
    )
  }
}
