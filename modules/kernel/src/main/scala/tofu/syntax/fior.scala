package tofu.syntax

import cats.{Functor, Monad, Semigroup}
import cats.data.Ior
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

object fior {

  implicit class TofuFIorOps[F[_], A, B](private val fior: F[Ior[A, B]]) extends AnyVal {

    def mapF[C](f: B => F[C])(implicit F: Monad[F]): F[Ior[A, C]] =
      fior.flatMap(_.traverse(f))

    def mapIn[C](f: B => C)(implicit F: Functor[F]): F[Ior[A, C]] =
      fior.map(_.map(f))

    def doubleFlatMap[C](f: B => F[Ior[A, C]])(implicit F: Monad[F], S: Semigroup[A]): F[Ior[A, C]] =
      fior.flatMap(_.flatTraverse(f))

    def doubleFlatTap[C](f: B => F[C])(implicit F: Monad[F]): F[Ior[A, B]] =
      fior.flatTap(_.traverse(f))
  }

}