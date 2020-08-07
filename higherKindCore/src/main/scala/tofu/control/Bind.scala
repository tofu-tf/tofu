package tofu.control

import cats.Bifunctor
import cats.Monad
import tofu.control.impl.BindInstanceChain

trait BiMonad[L[_, _], R[_, _]] {
  def left[A, B](a: A): L[A, B]
  def right[A, B](b: B): R[A, B]

  def leftFlatMap[A, B, C, D](lab: L[A, B])(fl: A => L[C, D], fr: B => R[C, D]): L[C, D]
  def rightFlatMap[A, B, C, D](rab: R[A, B])(fl: A => L[C, D], fr: B => R[C, D]): R[C, D]

  def leftBifunctor: Bifunctor[L] = new Bifunctor[L] {
    def bimap[A, B, C, D](fab: L[A, B])(f: A => C, g: B => D): L[C, D] =
      leftFlatMap(fab)(a => left(f(a)), b => right(g(b)))
  }

  def rightBifunctor: Bifunctor[R] = new Bifunctor[R] {
    def bimap[A, B, C, D](fab: R[A, B])(f: A => C, g: B => D): R[C, D] =
      rightFlatMap(fab)(a => left(f(a)), b => right(g(b)))
  }
}

trait TwinMonad[F[_, _]] extends BiMonad[F, F] with Bifunctor[F] { self =>
  def pure[E, A](a: A): F[E, A]

  def raise[E, A](e: E): F[E, A]

  def foldWith[E, A, X, R](fa: F[E, A], h: E => F[X, R], f: A => F[X, R]): F[X, R]

  final def foldWithC[E, A, X, R](fa: F[E, A])(h: E => F[X, R])(f: A => F[X, R]): F[X, R] =
    foldWith(fa, h, f)

  def fromEither[E, A](ea: Either[E, A]): F[E, A] = ea match {
    case Left(e)  => raise(e)
    case Right(a) => pure(a)
  }

  def fold[E, A, X, R](fa: F[E, A])(h: E => R, f: A => R): F[X, R] =
    foldWith[E, A, X, R](fa, e => pure(h(e)), a => pure(f(a)))

  def flatMap[E, A, B](fa: F[E, A], f: A => F[E, B]): F[E, B] =
    foldWith(fa, raise[E, B], f)

  def map[E, A, B](fa: F[E, A])(f: A => B): F[E, B] = flatMap(fa, (a: A) => pure(f(a)))

  def as[E, A, B](fa: F[E, A])(b: => B): F[E, B] = map(fa)(_ => b)

  def void[E, A](fa: F[E, A]): F[E, Unit] = map(fa)(_ => ())

  def flatMapErr[E, A, X](fa: F[E, A], f: E => F[X, A]): F[X, A] =
    foldWith(fa, f, pure[X, A])

  def mapErr[E, A, X](fa: F[E, A])(f: E => X): F[X, A] =
    flatMapErr(fa, (e: E) => raise(f(e)))

  def fail[E, A, B](fa: F[E, A])(f: A => E): F[E, B] =
    flatMap(fa, (a: A) => raise(f(a)))

  def handleWith[E, X, A](fa: F[E, A], h: E => F[X, A]): F[X, A] =
    foldWith(fa, h, pure[X, A])

  def handle[E, X, A](fa: F[E, A], h: E => A): F[X, A] =
    handleWith[E, X, A](fa, e => pure(h(e)))

  def errAs[E, A, X](fa: F[E, A])(x: => X) = mapErr(fa)(_ => x)

  def voidErr[E, A](fa: F[E, A]): F[Unit, A] = mapErr(fa)(_ => ())

  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
    foldWith[A, B, C, D](fab, a => raise(f(a)), b => pure(g(b)))

  def swapMap[E, A, X, B](fab: F[E, A])(f: E => B, g: A => X): F[X, B] =
    foldWith[E, A, X, B](fab, e => pure(f(e)), a => raise(g(a)))

  def swap[E, A](fab: F[E, A]): F[A, E] = foldWith[E, A, A, E](fab, e => pure(e), a => raise(a))

  def left[A, B](a: A): F[A, B] = raise(a)

  def right[A, B](b: B): F[A, B] = pure(b)

  def leftFlatMap[A, B, C, D](lab: F[A, B])(fl: A => F[C, D], fr: B => F[C, D]): F[C, D] = foldWith(lab, fl, fr)

  def rightFlatMap[A, B, C, D](rab: F[A, B])(fl: A => F[C, D], fr: B => F[C, D]): F[C, D] = foldWith(rab, fl, fr)

  def bifunctor: Bifunctor[F] = new Bifunctor[F] {
    def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] = self.bimap(fab)(f, g)
  }

  override def leftBifunctor: Bifunctor[F] = bifunctor

  override def rightBifunctor: Bifunctor[F] = bifunctor
}

object TwinMonad extends BindInstanceChain[TwinMonad]

trait Bind[F[_, _]] extends TwinMonad[F] { self =>
  def foldRec[E, A, X, B](init: Either[E, A])(step: Either[E, A] => F[Either[E, X], Either[A, B]]): F[X, B]

  def tailRecMap[E, A, B](a: A)(f: A => F[E, Either[A, B]]): F[E, B] =
    foldRec[Nothing, A, E, B](Right(a)) { ea => mapErr(f(ea.merge))(Right(_)) }

  def tailRecHandle[R, A, B](a: A)(f: A => F[Either[A, B], R]): F[B, R] =
    foldRec[A, Nothing, B, R](Left(a)) { ea => map(f(ea.merge))(Right(_)) }

  def monad[E]: Monad[F[E, *]] = new Monad[F[E, *]] {
    def pure[A](x: A): F[E, A] = self.pure(x)

    def flatMap[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B] = self.flatMap(fa, f)

    def tailRecM[A, B](a: A)(f: A => F[E, Either[A, B]]): F[E, B] = self.tailRecMap(a)(f)
  }

  def lmonad[R]: Monad[F[*, R]] = new Monad[F[*, R]] {
    override def flatMap[A, B](fa: F[A, R])(f: A => F[B, R]): F[B, R] = self.handleWith(fa, f)

    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B], R]): F[B, R] = self.tailRecHandle(a)(f)

    override def pure[A](x: A): F[A, R] = self.raise(x)
  }
}

object Bind extends BindInstanceChain[Bind] {
  def pure[F[_, _]] = new PureApp[F](true)
  class PureApp[F[_, _]](private val __ : Boolean) extends AnyVal {
    def apply[A](a: A)(implicit F: TwinMonad[F]): F[Nothing, A] = F.pure(a)
  }

  def raise[F[_, _]] = new RaiseApp[F](true)
  class RaiseApp[F[_, _]](private val __ : Boolean) extends AnyVal {
    def apply[E](a: E)(implicit F: TwinMonad[F]): F[E, Nothing] = F.raise(a)
  }
}

trait StackSafeBind[F[_, _]] extends Bind[F] { self =>
  override def foldRec[E, A, X, B](init: Either[E, A])(step: Either[E, A] => F[Either[E, X], Either[A, B]]): F[X, B] =
    foldWith[Either[E, X], Either[A, B], X, B](
      step(init),
      {
        case Left(e)  => foldRec(Left(e))(step)
        case Right(x) => raise(x)
      },
      {
        case Left(a)  => foldRec(Right(a))(step)
        case Right(b) => pure(b)
      }
    )
}
