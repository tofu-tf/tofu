package tofu.syntax

import cats.{Functor, Monad, Semigroup}
import cats.data.Ior
import cats.syntax.flatMap._
import cats.syntax.functor._

object fior {

  implicit class TofuFIorOps[F[_], A, B](private val fior: F[Ior[A, B]]) extends AnyVal {

    def mapF[C](f: B => F[C])(implicit F: Monad[F]): F[Ior[A, C]] =
      fior.flatMap(_.traverse(f))

    def mapIn[C](f: B => C)(implicit F: Functor[F]): F[Ior[A, C]] =
      fior.map(_.map(f))

    def flatMapIn[AA >: A, C](f: B => Ior[AA, C])(implicit F: Functor[F], s: Semigroup[AA]): F[Ior[AA, C]] =
      fior.map(_.flatMap(f))

    def doubleFlatMap[AA >: A, C](f: B => F[Ior[AA, C]])(implicit F: Monad[F], S: Semigroup[AA]): F[Ior[AA, C]] =
      fior.flatMap {
        case l @ Ior.Left(_) => F.pure(l)
        case Ior.Right(b) => f(b)
        case Ior.Both(a0, b) => f(b).map {
          case Ior.Left(a) => Ior.left(S.combine(a0, a))
          case Ior.Right(b) => Ior.both(a0, b)
          case Ior.Both(a, b) => Ior.both(S.combine(a0, a), b)
        }
      }

    def doubleFlatTap[C](f: B => F[C])(implicit F: Monad[F]): F[Ior[A, B]] =
      fior.flatTap(_.traverse(f))
  }

}