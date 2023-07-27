package tofu.syntax

import tofu.control.TwinMonad

object bind {
  implicit class BindSyntax[F[+_, +_], E, A](private val self: F[E, A]) extends AnyVal {
    def flatMap[F1[e, a] >: F[e, a], E1 >: E, B](f: A => F1[E1, B])(implicit F: TwinMonad[F1]): F1[E1, B] =
      F.flatMap(self, f)

    def flatTap[F1[e, a] >: F[e, a], E1 >: E, B](f: A => F1[E1, B])(implicit F: TwinMonad[F1]): F1[E1, A] =
      F.flatTap(self, f)

    def map[B](f: A => B)(implicit F: TwinMonad[F]): F[E, B] = F.map(self)(f)

    def tapBoth[F1[e, a] >: F[e, a], X, B, Y, C](h: E => F1[X, B], f: A => F1[Y, C])(implicit
        F: TwinMonad[F1]
    ): F1[E, A] = F.tapBoth(self)(h, f)

    def *>[F1[e, a] >: F[e, a], E1 >: E, B](fb: F1[E1, B])(implicit F: TwinMonad[F1]): F1[E1, B] = flatMap(_ => fb)

    def mapErr[X](f: E => X)(implicit F: TwinMonad[F]): F[X, A] = F.mapErr(self)(f)

    def handleWith[F1[e, a] >: F[e, a], X, A1 >: A](f: E => F1[X, A1])(implicit F: TwinMonad[F1]): F1[X, A1] =
      F.handleWith(self, f)

    def handleTap[F1[e, a] >: F[e, a], X, A1 >: A](f: E => F1[X, A1])(implicit F: TwinMonad[F1]): F1[E, A1] =
      F.handleTap(self, f)

    def handle[A1 >: A](f: E => A1)(implicit F: TwinMonad[F]): F[Nothing, A1] = F.handle(self, f)

    def foldWith[F1[e, a] >: F[e, a], X, R](h: E => F1[X, R], f: A => F1[X, R])(implicit F: TwinMonad[F1]): F1[X, R] =
      F.foldWith(self)(h, f)

    def fold[R](h: E => R, f: A => R)(implicit F: TwinMonad[F]): F[Nothing, R] = F.fold(self)(h, f)

    def as[B](b: B)(implicit F: TwinMonad[F]): F[E, B] = F.as(self)(b)

    def void(implicit F: TwinMonad[F]): F[E, Unit] = F.void(self)

    def flatMapErr[F1[e, a] >: F[e, a], X](f: E => F1[X, A])(implicit F: TwinMonad[F1]): F1[X, A] =
      F.flatMapErr(self, f)

    def fail[B](f: A => E)(implicit F: TwinMonad[F]): F[E, B] = F.fail(self)(f)

    def errAs[X](x: => X)(implicit F: TwinMonad[F]) = F.errAs(self)(x)

    def voidErr(implicit F: TwinMonad[F]): F[Unit, A] = F.voidErr(self)

    def bimap[X, R](f: E => X, g: A => R)(implicit F: TwinMonad[F]): F[X, R] = F.bimap(self)(f, g)

    def swapMap[X, B](f: E => B, g: A => X)(implicit F: TwinMonad[F]): F[X, B] =
      F.swapMap(self)(f, g)

    def swap(implicit F: TwinMonad[F]): F[A, E] = F.swap(self)
  }
}

object bindInv {
  implicit class BindInvariantSyntax[F[+_, +_], E, A](private val self: F[E, A]) extends AnyVal {
    def flatMap[E1 >: E, B](f: A => F[E1, B])(implicit F: TwinMonad[F]): F[E1, B] =
      F.flatMap(self, f)

    def flatTap[E1 >: E, B](f: A => F[E1, B])(implicit F: TwinMonad[F]): F[E1, A] =
      F.flatTap(self, f)

    def map[B](f: A => B)(implicit F: TwinMonad[F]): F[E, B] = F.map(self)(f)

    def tapBoth[X, B, Y, C](h: E => F[X, B], f: A => F[Y, C])(implicit
        F: TwinMonad[F]
    ): F[E, A] = F.tapBoth(self)(h, f)

    def *>[E1 >: E, B](fb: F[E1, B])(implicit F: TwinMonad[F]): F[E1, B] = flatMap(_ => fb)

    def mapErr[X](f: E => X)(implicit F: TwinMonad[F]): F[X, A] = F.mapErr(self)(f)

    def handleWith[X, A1 >: A](f: E => F[X, A1])(implicit F: TwinMonad[F]): F[X, A1] =
      F.handleWith(self, f)

    def handleTap[X, A1 >: A](f: E => F[X, A1])(implicit F: TwinMonad[F]): F[E, A1] =
      F.handleTap(self, f)

    def handle[A1 >: A](f: E => A1)(implicit F: TwinMonad[F]): F[Nothing, A1] = F.handle(self, f)

    def foldWith[X, R](h: E => F[X, R], f: A => F[X, R])(implicit F: TwinMonad[F]): F[X, R] = F.foldWith(self)(h, f)

    def fold[R](h: E => R, f: A => R)(implicit F: TwinMonad[F]): F[Nothing, R] = F.fold(self)(h, f)

    def as[B](b: B)(implicit F: TwinMonad[F]): F[E, B] = F.as(self)(b)

    def void(implicit F: TwinMonad[F]): F[E, Unit] = F.void(self)

    def flatMapErr[X](f: E => F[X, A])(implicit F: TwinMonad[F]): F[X, A] =
      F.flatMapErr(self, f)

    def fail[B](f: A => E)(implicit F: TwinMonad[F]): F[E, B] = F.fail(self)(f)

    def errAs[X](x: => X)(implicit F: TwinMonad[F]) = F.errAs(self)(x)

    def voidErr(implicit F: TwinMonad[F]): F[Unit, A] = F.voidErr(self)

    def bimap[X, R](f: E => X, g: A => R)(implicit F: TwinMonad[F]): F[X, R] = F.bimap(self)(f, g)

    def swapMap[X, B](f: E => B, g: A => X)(implicit F: TwinMonad[F]): F[X, B] =
      F.swapMap(self)(f, g)

    def swap(implicit F: TwinMonad[F]): F[A, E] = F.swap(self)
  }
}
