package tofu.control.impl
import tofu.control.Bind

import cats.Monad

import scala.annotation.tailrec
import cats.Bifunctor
import cats.data.EitherT
import tofu.syntax.monadic._

trait BindInstanceChain[TC[f[_, _]] >: Bind[f]] {
  implicit val eitherInstance: TC[Either] = new Bind[Either] {
    import cats.instances.either._

    def pure[E, A](a: A): Either[E, A] = Right(a)

    def raise[E, A](e: E): Either[E, A] = Left(e)

    def foldWith[E, A, X, R](fa: Either[E, A], h: E => Either[X, R], f: A => Either[X, R]): Either[X, R] = fa.fold(h, f)

    def foldRec[E, A, X, B](init: Either[E, A])(
        step: Either[E, A] => Either[Either[E, X], Either[A, B]]
    ): Either[X, B] = {
      @tailrec def go(e: Either[E, A]): Either[X, B] = step(e) match {
        case Left(Left(e))   => go(Left(e))
        case Left(Right(x))  => Left(x)
        case Right(Left(a))  => go(Right(a))
        case Right(Right(b)) => Right(b)
      }

      go(init)
    }

    override def fromEither[E, A](ea: Either[E, A]): Either[E, A] = ea

    override def fold[E, A, X, R](fa: Either[E, A])(h: E => R, f: A => R): Either[X, R] = Right(fa.fold(h, f))

    override def handle[E, X, A](fa: Either[E, A], h: E => A): Either[X, A] = Right(fa.fold(h, identity))

    override def monad[E]: Monad[Either[E, *]] = implicitly

    override def bifunctor: Bifunctor[Either] = implicitly
  }

  implicit def eitherTInstance[F[_]](implicit F: Monad[F]): TC[EitherT[F, *, *]] = new Bind[EitherT[F, *, *]] {
    def foldRec[E, A, X, B](init: Either[E, A])(
        step: Either[E, A] => EitherT[F, Either[E, X], Either[A, B]]
    ): EitherT[F, X, B] =
      EitherT(init.tailRecM {
        step(_).value.map {
          case Left(Left(e))   => Left(Left(e))
          case Left(Right(x))  => Right(Left(x))
          case Right(Left(a))  => Left(Right(a))
          case Right(Right(b)) => Right(Right(b))
        }
      })

    def pure[E, A](a: A): EitherT[F, E, A] = EitherT.rightT(a)

    def raise[E, A](e: E): EitherT[F, E, A] = EitherT.leftT(e)

    def foldWith[E, A, X, R](
        fa: EitherT[F, E, A],
        h: E => EitherT[F, X, R],
        f: A => EitherT[F, X, R]
    ): EitherT[F, X, R] =
      fa.biflatMap(h, f)

    override def fromEither[E, A](ea: Either[E, A]): EitherT[F, E, A] = EitherT.fromEither(ea)

    override def monad[E]: Monad[EitherT[F, E, *]] = implicitly

    override val bifunctor: Bifunctor[EitherT[F, *, *]] = implicitly
  }
}
