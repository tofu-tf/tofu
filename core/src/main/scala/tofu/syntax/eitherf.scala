package tofu.syntax

import cats.{Applicative, Functor, Monad, Traverse, ~>}
import cats.syntax.either._
import tofu.syntax.either._
import cats.instances.either.catsStdInstancesForEither
import tofu.Raise
import tofu.lift.Lift

object eitherf {

  implicit final class EitherFOps[F[_], L, R](private val e: F[Either[L, R]]) extends AnyVal {

    def orElseF[L1 >: L, R1 >: R](f: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      F.flatMap(e) {
        case r: Right[L, R] => F.pure(r)
        case _              => f
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

    def absolve(implicit R: Raise[F, L], F: Monad[F]): F[R] = {
      F.flatMap(e) {
        case Right(right) => F.pure(right)
        case Left(left)   => R.raise(left)
      }
    }

    def assocR[A, B](implicit F: Functor[F], ev: R <:< Either[A, B]): F[Either[Either[L, A], B]] = {
      F.map(e) {
        case Right(r) =>
          ev(r) match {
            case Left(a)  => Left(Right(a))
            case Right(b) => Right(b)
          }
        case left: Left[L, R] => left.coerceR[A].asLeft[B]
      }
    }

    def assocL[A, B](implicit F: Functor[F], ev: L <:< Either[A, B]): F[Either[A, Either[B, R]]] = {
      F.map(e) {
        case r: Right[L, R] => r.coerceL[B].asRight[A]
        case Left(l) =>
          ev(l) match {
            case a: Left[A, B]  => a.coerceR[Either[B, R]]
            case Right(b) => b.asLeft[R].asRight[A]
          }
      }
    }

    def mapF[B](f: R => F[B])(implicit F: Monad[F]): F[Either[L, B]] = {
      F.flatMap(e) {
        case Right(right) => F.map(f(right))(Right(_))
        case Left(left)   => F.pure(Left(left))
      }
    }

    def leftMapF[L1](f: L => F[L1])(implicit F: Monad[F]): F[Either[L1, R]] = {
      F.flatMap(e) {
        case Left(left)   => F.map(f(left))(Left(_))
        case Right(right) => F.pure(Right(right))
      }
    }

    def flatMapIn[L1 >: L, B](f: R => Either[L1, B])(implicit F: Functor[F]): F[Either[L1, B]] = {
      F.map(e) {
        case Right(right) => f(right)
        case Left(left)   => Left(left)
      }
    }

    def leftFlatMapIn[L1, R1 >: R](f: L => Either[L1, R1])(implicit F: Functor[F]): F[Either[L1, R1]] = {
      F.map(e) {
        case Right(right) => Right(right)
        case Left(left)   => f(left)
      }
    }

    def flatMapF[L1 >: L, B](f: R => F[Either[L1, B]])(implicit F: Monad[F]): F[Either[L1, B]] = {
      F.flatMap(e) {
        case Right(right) => f(right)
        case Left(left)   => F.pure(Left(left))
      }
    }

    def leftFlatMapF[R1 >: R, L1](f: L => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      F.flatMap(e) {
        case Left(left)   => f(left)
        case Right(right) => F.pure(Right(right))
      }
    }

    def doubleFlatMap[L1 >: L, R1 >: R](f: R => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      F.flatMap(e) {
        case Right(right) => f(right)
        case Left(left)   => F.pure(Left(left))
      }
    }

    def swapF(implicit F: Functor[F]): F[Either[R, L]] = {
      F.map(e)(_.swap)
    }

    def ensure[L1 >: L](f: R => Boolean, err: => L1)(implicit F: Functor[F]): F[Either[L1, R]] = {
      F.map(e) {
        case Right(right) => Either.cond(f(right), right, err)
        case left         => left
      }
    }

    def ensureF[L1 >: L](f: R => F[Boolean], err: => F[L1])(implicit F: Monad[F]): F[Either[L1, R]] = {
      F.flatMap(e) {
        case Right(right) => F.ifM(f(right))(F.pure(Right(right)), F.map(err)(Left(_)))
        case left         => F.pure(left)
      }
    }

    def traverseF[G[_]: Applicative, B](f: R => G[B])(implicit F: Functor[F]): F[G[Either[L, B]]] = {
      F.map(e)(_.traverse(f))
    }

    def traverseAll[G[_]: Applicative, B](f: R => G[B])(implicit F: Traverse[F]): G[F[Either[L, B]]] = {
      F.traverse(e)(_.traverse(f))
    }

    def leftTraverseF[G[_], B](f: L => G[B])(implicit F: Functor[F], G: Applicative[G]): F[G[Either[B, R]]] = {
      F.map(e) {
        case Left(left) => G.map(f(left))(Left(_))
        case Right(r)   => G.pure(Right(r))
      }
    }

    def leftTraverseAll[G[_], B](f: L => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[F[Either[B, R]]] = {
      F.traverse(e) {
        case Left(left) => G.map(f(left))(Left(_))
        case Right(r)   => G.pure(Right(r))
      }
    }

    def productF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Applicative[F]): F[Either[L1, (R, R1)]] = {
      F.compose[Either[L1, *]].product(F.map(e)(_.leftMap(identity(_: L1))), eb)
    }

    def productRF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Applicative[F]): F[Either[L1, R1]] = {
      F.compose[Either[L1, *]].productR(F.map(e)(_.leftMap(identity(_: L1))))(eb)
    }

    def productLF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Applicative[F]): F[Either[L1, R]] = {
      F.compose[Either[L1, *]].productL(F.map(e)(_.leftMap(identity(_: L1))))(eb)
    }

    def apF[L1 >: L, R1, Z](eb: => F[Either[L1, R1]])(implicit F: Applicative[F], ev: R <:< (R1 => Z)): F[Either[L1, Z]] = {
      F.compose[Either[L1, *]].ap(F.map(e)(_.map(ev)))(eb)
    }

    def map2F[L1 >: L, R1, Z](eb: => F[Either[L1, R1]])(f: (R, R1) => Z)(implicit F: Applicative[F]): F[Either[L1, Z]] = {
      F.compose[Either[L1, *]].map2(F.map(e)(_.leftMap(identity(_: L1))), eb)(f)
    }

    def flatMap2F[L1 >: L, R1, Z](eb: => F[Either[L1, R1]])(f: (R, R1) => F[Z])(implicit F: Monad[F]): F[Either[L1, Z]] = {
      F.flatMap(e) {
        case Right(a) =>
          F.flatMap(eb) {
            case Right(b) => F.map(f(a, b))(Right(_))
            case left => F.pure(left)
          }
        case Left(left) => F.pure(Left(left))
      }
    }

    def mapK[G[_]](implicit F: ~>[F, G]): G[Either[L, R]] = {
      F(e)
    }

    def liftTo[G[_]](implicit F: Lift[F, G]): G[Either[L, R]] = {
      F.lift(e)
    }
  }

  implicit final class EitherIdFOps[A](private val id: A) extends AnyVal {
    def asRightF[F[_], L](implicit F: Applicative[F]): F[Either[L, A]] = F.pure(Right[L, A](id))

    def asLeftF[F[_], R](implicit F: Applicative[F]): F[Either[A, R]] = F.pure(Left[A, R](id))
  }


  implicit final class EitherFObjectOps(private val o: Either.type) extends AnyVal {
    def condF[F[_], L, R](test: Boolean, r: => F[R], l: => F[L])(implicit F: Functor[F]): F[Either[L, R]] = {
      if (test)
        F.map(r)(Either.right[L, R])
      else
        F.map(l)(Either.left[L, R])
    }
  }
}
