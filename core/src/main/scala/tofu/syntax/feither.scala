package tofu.syntax

import cats.syntax.either._
import cats.{Applicative, Functor, Monad, Traverse, ~>}
import tofu.Raise
import tofu.lift.Lift

object feither {

  implicit final class EitherFOps[F[_], L, R](private val e: F[Either[L, R]]) extends AnyVal {

    def orElseF[L1 >: L, R1 >: R](f: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      F.flatMap(e) {
        case r @ Right(_) => F.pure(r.leftCast)
        case _            => f
      }
    }

    def getOrElseF[R1 >: R](f: => F[R1])(implicit F: Monad[F]): F[R1] = {
      F.flatMap(e) {
        case Right(value) => F.pure(value)
        case _            => f
      }
    }

    def catchAll[R1 >: R](f: L => F[R1])(implicit F: Monad[F]): F[R1] = {
      F.flatMap(e) {
        case Left(err)    => f(err)
        case Right(right) => F.pure(right)
      }
    }

    def absolve[R1 >: R](implicit R: Raise[F, L], F: Monad[F]): F[R1] = {
      F.flatMap(e) {
        case Right(right) => F.pure(right)
        case Left(left)   => R.raise(left)
      }
    }

    def assocR[A, B](implicit F: Functor[F], ev: R <:< Either[A, B]): F[Either[Either[L, A], B]] = {
      F.map(e) {
        case Right(r) =>
          ev(r) match {
            case Left(a)      => Left(Right(a))
            case b @ Right(_) => b.leftCast
          }
        case left @ Left(_) => left.rightCast.asLeft
      }
    }

    def assocL[A, B](implicit F: Functor[F], ev: L <:< Either[A, B]): F[Either[A, Either[B, R]]] = {
      F.map(e) {
        case r @ Right(_) => r.leftCast.asRight
        case Left(l) =>
          ev(l) match {
            case left @ Left(_) => left.rightCast
            case Right(b)       => b.asLeft.asRight
          }
      }
    }

    def mapF[B](f: R => F[B])(implicit F: Monad[F]): F[Either[L, B]] = {
      F.flatMap(e)(_.traverse(f))
    }

    def leftMapF[L1](f: L => F[L1])(implicit F: Monad[F]): F[Either[L1, R]] = {
      F.flatMap(e) {
        case Left(left)       => F.map(f(left))(_.asLeft)
        case right @ Right(_) => F.pure(right.leftCast)
      }
    }

    def flatMapIn[L1 >: L, B](f: R => Either[L1, B])(implicit F: Functor[F]): F[Either[L1, B]] = {
      F.map(e)(_.flatMap(f))
    }

    def leftFlatMapIn[L1, R1 >: R](f: L => Either[L1, R1])(implicit F: Functor[F]): F[Either[L1, R1]] = {
      F.map(e) {_.leftFlatMap(f)
      }
    }

    def flatMapF[L1 >: L, B](f: R => F[Either[L1, B]])(implicit F: Monad[F]): F[Either[L1, B]] = {
      F.flatMap(e) {
        case Right(right)   => f(right)
        case left @ Left(_) => F.pure(left.rightCast)
      }
    }

    def leftFlatMapF[R1 >: R, L1](f: L => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      F.flatMap(e) {
        case Left(left)       => f(left)
        case right @ Right(_) => F.pure(right.leftCast)
      }
    }

    def doubleFlatMap[L1 >: L, R1 >: R](f: R => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      F.flatMap(e) {
        case Right(right)   => f(right)
        case left @ Left(_) => F.pure(left.rightCast)
      }
    }

    def swapF(implicit F: Functor[F]): F[Either[R, L]] = {
      F.map(e)(_.swap)
    }

    def ensure[L1 >: L](f: R => Boolean, err: => L1)(implicit F: Functor[F]): F[Either[L1, R]] = {
      flatMapIn(right => Either.cond(f(right), right, err))
    }

    def ensureF[L1 >: L](f: R => F[Boolean], err: => F[L1])(implicit F: Monad[F]): F[Either[L1, R]] = {
      flatMapF(right => F.flatMap(f(right))(p => Either.condF(p, F.pure(right), err)))
    }

    def traverseF[G[_]: Applicative, R1](f: R => G[R1])(implicit F: Functor[F]): F[G[Either[L, R1]]] = {
      F.map(e)(_.traverse(f))
    }

    def traverseAll[G[_]: Applicative, R1](f: R => G[R1])(implicit F: Traverse[F]): G[F[Either[L, R1]]] = {
      F.traverse(e)(_.traverse(f))
    }

    def leftTraverseF[G[_], L1](f: L => G[L1])(implicit F: Functor[F], G: Applicative[G]): F[G[Either[L1, R]]] = {
      F.map(e) {
        case Left(left)   => G.map(f(left))(_.asLeft)
        case r @ Right(_) => G.pure(r.leftCast)
      }
    }

    def leftTraverseAll[G[_], L1](f: L => G[L1])(implicit F: Traverse[F], G: Applicative[G]): G[F[Either[L1, R]]] = {
      F.traverse(e) {
        case Left(left)   => G.map(f(left))(_.asLeft)
        case r @ Right(_) => G.pure(r.leftCast)
      }
    }

    def productF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, (R, R1)]] = {
      F.flatMap(e) {
        case Right(r) =>
          F.flatMap(eb) {
            case Right(r1)      => (r, r1).asRightF
            case left @ Left(_) => F.pure(left.rightCast)
          }
        case left @ Left(_) => F.pure(left.rightCast)
      }
    }

    def productRF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      F.map(productF(eb))(_.map(_._2))
    }

    def productLF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R]] = {
      F.map(productF(eb))(_.map(_._1))
    }

    def apF[L1 >: L, R1, Z](
        eb: => F[Either[L1, R1]]
    )(implicit F: Monad[F], ev: R <:< (R1 => Z)): F[Either[L1, Z]] = {
      F.flatMap(e) {
        case Right(r) =>
          F.flatMap(eb) {
            case Right(r1)      => r(r1).asRightF
            case left @ Left(_) => F.pure(left.rightCast)
          }
        case left @ Left(_) => F.pure(left.rightCast)
      }
    }

    def map2F[L1 >: L, R1, Z](
        eb: => F[Either[L1, R1]]
    )(f: (R, R1) => Z)(implicit F: Monad[F]): F[Either[L1, Z]] = {
      F.map(productF(eb))(_.map { case (r, r1) => f(r, r1) })
    }

    def flatMap2F[L1 >: L, R1, Z](
        eb: => F[Either[L1, R1]]
    )(f: (R, R1) => F[Z])(implicit F: Monad[F]): F[Either[L1, Z]] = {
      F.flatMap(productF(eb))(_.traverse { case (r, r1) => f(r, r1) })
    }

    def mapK[G[_]](implicit F: ~>[F, G]): G[Either[L, R]] = {
      F(e)
    }

    def liftTo[G[_]](implicit F: Lift[F, G]): G[Either[L, R]] = {
      F.lift(e)
    }

    def mergeF(implicit ev: L =:= R, F: Functor[F]): F[R] = {
      F.map(e) {
        case Left(value) => ev(value)
        case Right(value) => value
      }
    }
  }

  implicit final class EitherIdFOps[A](private val id: A) extends AnyVal {
    def asRightF[F[_], L](implicit F: Applicative[F]): F[Either[L, A]] = F.pure(id.asRight)

    def asLeftF[F[_], R](implicit F: Applicative[F]): F[Either[A, R]] = F.pure(id.asLeft)
  }

  implicit final class EitherFObjectOps(private val o: Either.type) extends AnyVal {
    def condF[F[_], L, R](test: Boolean, r: => F[R], l: => F[L])(implicit F: Functor[F]): F[Either[L, R]] = {
      if (test)
        F.map(r)(_.asRight)
      else
        F.map(l)(_.asLeft)
    }
  }
}
