package tofu.syntax

import cats.{Applicative, Functor, Monad, Traverse}
import cats.syntax.traverse.toTraverseOps
import cats.instances.option.catsStdInstancesForOption
import cats.syntax.option._
import tofu.Raise
import tofu.syntax.monadic._
import tofu.syntax.feither.EitherIdFOps

object foption {
  def noneF[F[_]: Applicative, A]: F[Option[A]] = none[A].pure[F]

  implicit final class FOptionOps[A](private val a: A) extends AnyVal {
    def someF[F[_]: Applicative]: F[Option[A]] = a.some.pure[F]
  }

  implicit final class FOptionSyntax[F[_], A](private val lhs: F[Option[A]]) extends AnyVal {
    def getOrElseF[B >: A](fa: => F[B])(implicit F: Monad[F]): F[B] =
      lhs.flatMap(_.fold(fa)(F.pure))

    def orElseF(fa: => F[Option[A]])(implicit F: Monad[F]): F[Option[A]] =
      lhs.flatMap {
        case None => fa
        case x    => x.pure[F]
      }

    def orThrow[E](err: => E)(implicit F: Monad[F], FE: Raise[F, E]): F[A] =
      lhs.getOrElseF(FE.raise(err))

    def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): F[Option[B]] =
      lhs.flatMapF(f(_).map(_.some))

    def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): F[Option[B]] =
      lhs.flatMap(_.fold(noneF[F, B])(f(_)))

    def toRightF[B](left: => F[B])(implicit F: Monad[F]): F[Either[B, A]] =
      lhs.flatMap {
        case Some(value) => value.asRightF[F, B]
        case _           => left.map(Left(_))
      }

    def toLeftF[B](right: => F[B])(implicit F: Monad[F]): F[Either[A, B]] =
      lhs.flatMap {
        case Some(value) => value.asLeftF[F, B]
        case None        => right.map(Right(_))
      }

    def filterIn(f: A => Boolean)(implicit F: Functor[F]): F[Option[A]] =
      lhs.map(_.filter(f))

    def filterF(f: A => F[Boolean])(implicit F: Monad[F]): F[Option[A]] =
      lhs.flatMap {
        case None => noneF[F, A]
        case s @ Some(value) =>
          f(value).map {
            case true => s
            case _    => None
          }
      }

    def ensure[B](f: A => Boolean, err: => B)(implicit F: Functor[F]): F[Either[B, A]] =
      lhs.map {
        case Some(value) => Either.cond(f(value), value, err)
        case None        => Left(err)
      }

    def ensureF[B](f: A => F[Boolean], err: => F[B])(implicit F: Monad[F]): F[Either[B, A]] =
      lhs.flatMap {
        case Some(value) => f(value).ifM(value.asRightF[F, B], err.map(Left(_)))
        case None        => err.map(Left(_))
      }

    def traverseF[G[_]: Applicative, B](f: A => G[B])(implicit F: Functor[F]): F[G[Option[B]]] =
      lhs.map(_.traverse(f))

    def traverseAll[G[_]: Applicative, B](f: A => G[B])(implicit F: Traverse[F]): G[F[Option[B]]] =
      lhs.traverse(_.traverse(f))

    def productF[B](f: => F[Option[B]])(implicit F: Monad[F]): F[Option[(A, B)]] = {
      lhs.flatMap(_.flatTraverse(a => f.map(_.tupleLeft(a))))
    }

    def productRF[B](f: => F[Option[B]])(implicit F: Monad[F]): F[Option[B]] =
      productF(f).map(_.map(_._2))

    def productLF[B](f: => F[Option[B]])(implicit F: Monad[F]): F[Option[A]] =
      productF(f).map(_.map(_._1))

    def apF[X, Z](f: => F[Option[X]])(implicit F: Monad[F], ev: A <:< (X => Z)): F[Option[Z]] =
      lhs.flatMap(_.flatTraverse(fn => f.map(_.map(fn))))

    def map2F[B, Z](fb: => F[Option[B]])(f: (A, B) => Z)(implicit F: Monad[F]): F[Option[Z]] =
      productF(fb).map(_.map { case (a, b) => f(a, b) })

    def flatMap2F[B, Z](fb: => F[Option[B]])(f: (A, B) => F[Z])(implicit F: Monad[F]): F[Option[Z]] =
      productF(fb).flatMap(_.traverse { case (a, b) => f(a, b) })
  }
}
