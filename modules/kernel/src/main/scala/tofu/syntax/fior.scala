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
        case Ior.Right(b)    => f(b)
        case Ior.Both(a0, b) =>
          f(b).map {
            case Ior.Left(a)    => Ior.left(S.combine(a0, a))
            case Ior.Right(b)   => Ior.both(a0, b)
            case Ior.Both(a, b) => Ior.both(S.combine(a0, a), b)
          }
      }

    def flatTapF[C](f: B => F[C])(implicit F: Monad[F]): F[Ior[A, B]] =
      fior.flatTap(_.traverse(f))

    def doubleFlatTap[C](f: B => F[Ior[A, C]])(implicit F: Monad[F], S: Semigroup[A]): F[Ior[A, B]] = {
      fior.flatMap {
        case Ior.Left(_)     => fior
        case Ior.Right(b)    =>
          f(b).map {
            case Ior.Left(a)    => Ior.left(a)
            case Ior.Right(_)   => Ior.right(b)
            case Ior.Both(a, _) => Ior.both(a, b)
          }
        case Ior.Both(a0, b) =>
          f(b).map {
            case Ior.Left(a)    => Ior.left(S.combine(a0, a))
            case Ior.Right(_)   => Ior.both(a0, b)
            case Ior.Both(a, _) => Ior.both(S.combine(a0, a), b)
          }
      }
    }
  }

}
