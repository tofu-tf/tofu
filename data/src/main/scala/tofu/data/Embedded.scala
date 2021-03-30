package tofu.data

import cats._
import cats.syntax.parallel._
import tofu.syntax.monadic._
import tofu.syntax.feither._
import tofu.syntax.funk
import ExceptTInstances1._
import cats.data.EitherT
import tofu.control.Bind

object Embedded extends ExceptTInstances {
  trait EmbedTag extends Any

  type Base = Any { type EmbedOpaque }
  type T[+F[+_], +G[+_], +A] <: Base with EmbedTag

  def apply[F[+_], G[+_], A](tfa: F[G[A]]): T[F, G, A] = tfa.asInstanceOf[T[F, G, A]]

  implicit final class EmbedOps[F[+_], G[+_], A](private val e: T[F, G, A]) extends AnyVal {
    @inline def value: F[G[A]] = e.asInstanceOf[F[G[A]]]
  }
}

object ExceptT {
  def fromEitherT[F[+_], E, A](fa: EitherT[F, E, A]): ExceptT[F, E, A] = Embedded(fa.value)
}

trait ExceptTInstances extends ExceptTInstances1 {
  implicit def exceptTVeryParallel[G[+_], E](implicit
      G: Parallel[G],
      E: Semigroup[E]
  ): Parallel.Aux[ExceptT[G, E, *], ExceptTPar[G.F, E, *]] =
    new Parallel[ExceptT[G, E, *]] {
      type F[A] = ExceptTPar[G.F, E, A]

      @inline private def par1 = funk.funKFrom[ExceptT[G, E, *]](fx => toPar(G.parallel(fx.value)))

      def parallel   = par1
      def sequential = funk.funKFrom[ExceptTPar[G.F, E, *]](fx => Embedded(G.sequential(fromPar(fx))))

      def monad = exceptTMonad[G, E](G.monad)

      def applicative = new ParApplicative[G.F, E]()(G.applicative, E)
    }
}
trait ExceptTInstances1 {
  implicit def exceptTMonad[F[+_], E](implicit F: Monad[F]): MonadError[ExceptT[F, E, *], E] =
    new MonadError[ExceptT[F, E, *], E] {
      override def map[A, B](fa: ExceptT[F, E, A])(f: A => B): ExceptT[F, E, B] =
        Embedded(fa.value.map(_.map(f)))

      def flatMap[A, B](fa: ExceptT[F, E, A])(f: A => ExceptT[F, E, B]): ExceptT[F, E, B] =
        Embedded(fa.value.flatMap {
          case l: Left[E, A] => (l.asInstanceOf[Left[E, B]]).pure[F]
          case Right(a)      => f(a).value
        })

      def tailRecM[A, B](a: A)(f: A => ExceptT[F, E, Either[A, B]]): ExceptT[F, E, B] =
        Embedded(F.tailRecM[A, Either[E, B]](a) { a1 =>
          f(a1).value.map {
            case Left(e)             => Right(Left(e))
            case Right(Left(a))      => Left(a)
            case r @ Right(Right(_)) => r.asInstanceOf[Right[A, Right[E, B]]]
          }
        })

      def pure[A](x: A): ExceptT[F, E, A] = Embedded(Right(x).pure[F])

      override val unit = pure(())

      def handleErrorWith[A](fa: ExceptT[F, E, A])(f: E => ExceptT[F, E, A]): ExceptT[F, E, A] =
        Embedded(fa.value.flatMap {
          case Left(e)        => f(e).value
          case r: Right[E, A] => r.pure[F]
        })

      def raiseError[A](e: E): ExceptT[F, E, A] = Embedded(e.asLeftF[F, A])
    }

  implicit def exceptTBind[F[+_]](implicit F: Monad[F]): Bind[ExceptT[F, *, *]] =
    new Bind[ExceptT[F, *, *]] {
      def pure[E, A](a: A): ExceptT[F, E, A] = Embedded(a.asRightF[F, E])

      def raise[E, A](e: E): ExceptT[F, E, A] = Embedded(e.asLeftF[F, A])

      def foldWith[E, A, X, R](fa: ExceptT[F, E, A])(
          h: E => ExceptT[F, X, R],
          f: A => ExceptT[F, X, R]
      ): ExceptT[F, X, R] = Embedded(fa.value.flatMap {
        case Left(e)  => h(e).value
        case Right(a) => f(a).value
      })

      def foldRec[E, A, X, B](init: Either[E, A])(
          step: Either[E, A] => ExceptT[F, Either[E, X], Either[A, B]]
      ): ExceptT[F, X, B] =
        Embedded {
          type FRR = Either[Either[E, A], Either[X, B]]

          F.tailRecM[Either[E, A], Either[X, B]](init) {
            step(_).value.map {
              case ll @ Left(Left(_))   => ll.asInstanceOf[FRR]
              case Left(Right(x))       => Right(Left(x))
              case Right(Left(a))       => Left(Right(a))
              case rr @ Right(Right(_)) => rr.asInstanceOf[FRR]
            }
          }
        }

    }

  implicit def exceptTParallel[G[+_], E: Semigroup](implicit
      G: Monad[G]
  ): Parallel.Aux[ExceptT[G, E, *], ExceptTPar[G, E, *]] =
    new Parallel[ExceptT[G, E, *]] {
      type F[A] = ExceptTPar[G, E, A]

      def parallel   = funk.funKFrom[ExceptT[G, E, *]](fx => toPar(fx.value))
      def sequential = funk.funK[ExceptTPar[G, E, *], ExceptT[G, E, *]](fx => fromPartoExcept(fx))

      def monad = exceptTMonad[G, E]

      def applicative = new ParApplicative[G, E]
    }
}

private[tofu] object ExceptTInstances1 {
  type ExceptTPar[F[_], E, A]
  @inline final def toPar[F[_], E, A](et: F[Either[E, A]])                = et.asInstanceOf[ExceptTPar[F, E, A]]
  @inline final def fromPar[F[_], E, A](et: ExceptTPar[F, E, A])          = et.asInstanceOf[F[Either[E, A]]]
  @inline final def fromPartoExcept[F[+_], E, A](et: ExceptTPar[F, E, A]) = et.asInstanceOf[ExceptT[F, E, A]]

  class ParApplicative[F[_]: Applicative, E: Semigroup] extends Applicative[ExceptTPar[F, E, *]] {
    def pure[A](x: A): ExceptTPar[F, E, A] = toPar(x.asRightF[F, E])

    def ap[A, B](ff: ExceptTPar[F, E, A => B])(fa: ExceptTPar[F, E, A]): ExceptTPar[F, E, B] = map2(ff, fa)(_(_))

    override def map2[A, B, C](fa: ExceptTPar[F, E, A], fb: ExceptTPar[F, E, B])(f: (A, B) => C): ExceptTPar[F, E, C] =
      toPar((fromPar(fa).map2(fromPar(fb))((ea, eb) => (ea, eb).parMapN(f))))
  }
}
