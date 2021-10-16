package tofu.syntax

import cats.instances.either._
import cats.syntax.either._
import cats.{Applicative, Functor, Monad, Traverse}
import cats.syntax.traverse._
import tofu.syntax.either._
import tofu.syntax.monadic._
import raise.FindRaise

object feither {

  implicit final class EitherFOps[F[_], L, R](private val e: F[Either[L, R]]) extends AnyVal {

    def orElseF[L1 >: L, R1 >: R](f: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      e.flatMap {
        case r: Right[L, R] => r.wideLeft[L1].wideRight[R1].pure[F]
        case _              => f
      }
    }

    def orElseIn[L1 >: L, R1 >: R](f: => Either[L1, R1])(implicit F: Functor[F]): F[Either[L1, R1]] = {
      e.map {
        case r: Right[L, R] => r.wideLeft[L1].wideRight[R1]
        case _              => f
      }
    }

    def getOrElseF[R1 >: R](f: => F[R1])(implicit F: Monad[F]): F[R1] = {
      e.flatMap(_.fold(_ => f, F.pure(_: R1)))
    }

    def catchAll[R1 >: R](f: L => F[R1])(implicit F: Monad[F]): F[R1] = {
      e.flatMap(_.fold(f, F.pure(_: R1)))
    }

    def absolve[R1 >: R](implicit R: FindRaise.Aux[L, F], F: Monad[F]): F[R1] = {
      e.flatMap(_.fold(FindRaise.unwrap(R).raise[R1], F.pure(_: R1)))
    }

    def assocR[A, B](implicit F: Functor[F], ev: R <:< Either[A, B]): F[Either[Either[L, A], B]] = {
      e.map {
        case Right(r)       =>
          ev(r) match {
            case Left(a)      => Left(Right(a))
            case b @ Right(_) => b.leftCast
          }
        case left @ Left(_) => left.rightCast.asLeft
      }
    }

    def assocL[A, B](implicit F: Functor[F], ev: L <:< Either[A, B]): F[Either[A, Either[B, R]]] = {
      e.map {
        case r @ Right(_) => r.leftCast.asRight
        case Left(l)      =>
          ev(l) match {
            case left @ Left(_) => left.rightCast
            case Right(b)       => b.asLeft.asRight
          }
      }
    }

    def mapF[B](f: R => F[B])(implicit F: Monad[F]): F[Either[L, B]] = {
      e.flatMap(_.traverse(f))
    }

    def tapF[B](f: R => F[B])(implicit F: Monad[F]): F[Either[L, R]] = {
      e.flatTap(_.traverse(f))
    }

    def mapIn[B](f: R => B)(implicit F: Functor[F]): F[Either[L, B]] =
      e.map(_.map(f))

    def leftMapF[L1](f: L => F[L1])(implicit F: Monad[F]): F[Either[L1, R]] = {
      e.flatMap {
        case Left(left)       => f(left).map(_.asLeft)
        case right @ Right(_) => right.leftCast[L1].pure[F]
      }
    }

    def leftTapF[B](f: L => F[B])(implicit F: Monad[F]): F[Either[L, R]] = {
      e.flatTap(_.swap.traverse(f))
    }

    def leftMapIn[B](f: L => B)(implicit F: Functor[F]): F[Either[B, R]] =
      e.map(_.left.map(f))

    def flatMapIn[L1 >: L, B](f: R => Either[L1, B])(implicit F: Functor[F]): F[Either[L1, B]] = {
      e.map(_.flatMap(f))
    }

    def leftFlatMapIn[L1, R1 >: R](f: L => Either[L1, R1])(implicit F: Functor[F]): F[Either[L1, R1]] = {
      e.map(_.leftFlatMap(f))
    }

    def leftFlatMapF[R1 >: R, L1](f: L => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      e.flatMap {
        case Left(left)       => f(left)
        case right @ Right(_) => right.leftCast[L1].wideRight[R1].pure[F]
      }
    }

    def doubleFlatMap[L1 >: L, R1](f: R => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      e.flatMap(_.wideLeft[L1].flatTraverse(f))
    }

    def swapF(implicit F: Functor[F]): F[Either[R, L]] = {
      F.map(e)(_.swap)
    }

    def ensure[L1 >: L](f: R => Boolean, err: => L1)(implicit F: Functor[F]): F[Either[L1, R]] = {
      flatMapIn(right => Either.cond(f(right), right, err))
    }

    def ensureF[L1 >: L](f: R => F[Boolean], err: => F[L1])(implicit F: Monad[F]): F[Either[L1, R]] = {
      doubleFlatMap(right => f(right).flatMap(p => Either.condF(p, right.pure[F], err)))
    }

    def traverseF[G[_]: Applicative, R1](f: R => G[R1])(implicit F: Functor[F]): F[G[Either[L, R1]]] = {
      e.map(_.traverse(f))
    }

    def traverseAll[G[_]: Applicative, R1](f: R => G[R1])(implicit F: Traverse[F]): G[F[Either[L, R1]]] = {
      F.traverse(e)(_.traverse(f))
    }

    def leftTraverseF[G[_]: Applicative, L1](f: L => G[L1])(implicit F: Functor[F]): F[G[Either[L1, R]]] = {
      e.map {
        case Left(left)   => f(left).map(_.asLeft)
        case r @ Right(_) => r.leftCast[L1].pure[G]
      }
    }

    def leftTraverseAll[G[_]: Applicative, L1](f: L => G[L1])(implicit F: Traverse[F]): G[F[Either[L1, R]]] = {
      e.traverse {
        case Left(left)   => f(left).map(_.asLeft)
        case r @ Right(_) => r.leftCast[L1].pure[G]
      }
    }

    def productF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, (R, R1)]] = {
      e.flatMap(_.wideLeft[L1].flatTraverse(r => eb.map(_.tupleLeft(r))))
    }

    def productRF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R1]] = {
      productF(eb).map(_.map(_._2))
    }

    def productLF[L1 >: L, R1](eb: => F[Either[L1, R1]])(implicit F: Monad[F]): F[Either[L1, R]] = {
      productF(eb).map(_.map(_._1))
    }

    def apF[L1 >: L, R1, Z](
        eb: => F[Either[L1, R1]]
    )(implicit F: Monad[F], ev: R <:< (R1 => Z)): F[Either[L1, Z]] = {
      e.flatMap(_.wideLeft[L1].flatTraverse(r => eb.map(_.map(r))))
    }

    def map2F[L1 >: L, R1, Z](
        eb: => F[Either[L1, R1]]
    )(f: (R, R1) => Z)(implicit F: Monad[F]): F[Either[L1, Z]] = {
      productF(eb).map(_.map(f.tupled))
    }

    def flatMap2F[L1 >: L, R1, Z](
        eb: => F[Either[L1, R1]]
    )(f: (R, R1) => F[Z])(implicit F: Monad[F]): F[Either[L1, Z]] = {
      productF(eb).flatMap(_.traverse(f.tupled))
    }

    def mergeF[A >: R](implicit ev: L <:< A, F: Functor[F]): F[A] = {
      e.map(_.fold(ev, identity(_: A)))
    }

    def reRaise(implicit R: FindRaise.Aux[L, F], M: Monad[F]): F[R] = FindRaise.unwrap(R).reRaise(e)
  }

  implicit final class EitherIdFOps[A](private val id: A) extends AnyVal {
    def asRightF[F[_]: Applicative, L]: F[Either[L, A]] = id.asRight[L].pure[F]

    def asLeftF[F[_]: Applicative, R]: F[Either[A, R]] = id.asLeft[R].pure[F]
  }

  implicit final class TofuEitherFOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def rightIn[L](implicit F: Functor[F]): F[Either[L, A]] = fa.map(_.asRight[L])

    def leftIn[R](implicit F: Functor[F]): F[Either[A, R]] = fa.map(_.asLeft[R])
  }

  implicit final class EitherFObjectOps(private val o: Either.type) extends AnyVal {
    def condF[F[_], L, R](test: Boolean, r: => F[R], l: => F[L])(implicit F: Functor[F]): F[Either[L, R]] = {
      if (test)
        r.map(_.asRight[L])
      else
        l.map(_.asLeft[R])
    }
  }
}
