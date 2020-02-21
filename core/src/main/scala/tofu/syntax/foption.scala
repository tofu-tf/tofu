package tofu.syntax

import cats.{Applicative, Monad}
import cats.syntax.option._
import tofu.Raise
import tofu.syntax.monadic._

object foption {
  def noneF[F[_]: Applicative, A]: F[Option[A]] = none[A].pure[F]

  implicit final class FOptionOps[A](private val a: A) extends AnyVal {
    def someF[F[_]: Applicative]: F[Option[A]] = a.some.pure[F]
  }

  implicit final class FOptionSyntax[F[_], A](private val lhs: F[Option[A]]) extends AnyVal {
    def getOrElseF(fa: => F[A])(implicit F: Monad[F]): F[A] =
      lhs.flatMap(_.fold(fa)(_.pure[F]))
    def orElseF(fa: => F[Option[A]])(implicit F: Monad[F]): F[Option[A]] =
      lhs.flatMap {
        case None => fa
        case x    => x.pure[F]
      }
    def orThrow[E](err: => E)(implicit F: Monad[F], FE: Raise[F, E]): F[A] =
      lhs.getOrElseF(FE.raise(err))
    def flatMapOpt[B](f: A => F[B])(implicit F: Monad[F]): F[Option[B]] =
      lhs.doubleFlatMap(f(_).map(_.some))
    def doubleFlatMap[B](f: A => F[Option[B]])(implicit F: Monad[F]): F[Option[B]] =
      lhs.flatMap(_.fold(none[B].pure[F])(f(_)))
  }
}
