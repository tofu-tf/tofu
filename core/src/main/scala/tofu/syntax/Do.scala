package tofu.syntax

import cats.data.EitherT
import cats.{Monad, Order}

import scala.collection.immutable.Queue
import scala.collection.{SortedMap, mutable}

class DoMonad[F[_]](val ! : Monad[F]) extends AnyVal

object DoMonad extends DoMonadInstances {
  final implicit def doMonad[F[_]](implicit F: Monad[F]): Do[F] = new DoMonad[F](F)

  def unit[F[_]](implicit F: Do[F]): F[Unit] = F.!.unit
  def pure[F[_]](implicit F: Do[F])          = new TofuDOPAPure(F.!)

  class TofuDOPAPure[F[_]](private val monad: Monad[F]) extends AnyVal {
    def apply[A](a: A): F[A] = monad.pure(a)
  }

  final class TofuDoOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Do[F]): F[B]                                     = F.!.map(fa)(f)
    def fmap[B](f: A => B)(implicit F: Do[F]): F[B]                                    = F.!.fmap(fa)(f)
    def widen[B >: A](implicit F: Do[F]): F[B]                                         = F.!.widen(fa)
    def void(implicit F: Do[F]): F[Unit]                                               = F.!.void(fa)
    def fproduct[B](f: A => B)(implicit F: Do[F]): F[(A, B)]                           = F.!.fproduct(fa)(f)
    def as[B](b: B)(implicit F: Do[F]): F[B]                                           = F.!.as(fa, b)
    def tupleLeft[B](b: B)(implicit F: Do[F]): F[(B, A)]                               = F.!.tupleLeft(fa, b)
    def tupleRight[B](b: B)(implicit F: Do[F]): F[(A, B)]                              = F.!.tupleRight(fa, b)
    def product[B](fb: F[B])(implicit F: Do[F]): F[(A, B)]                             = F.!.product(fa, fb)
    def productR[F1[x] >: F[x], B](fb: F1[B])(implicit F: Do[F1]): F1[B]               = F.!.productR(fa)(fb)
    def productL[F1[x] >: F[x], B](fb: F1[B])(implicit F: Do[F1]): F1[A]               = F.!.productL(fa)(fb)
    def *>[F1[x] >: F[x], B](fb: F1[B])(implicit F: Do[F1]): F1[B]                     = F.!.productR(fa)(fb)
    def <*[F1[x] >: F[x], B](fb: F[B])(implicit F: Do[F1]): F1[A]                      = F.!.productL(fa)(fb)
    def map2[F1[x] >: F[x], B, Z](fb: F[B])(f: (A, B) => Z)(implicit F: Do[F1]): F1[Z] = F.!.map2(fa, fb)(f)
    def map2Eval[F1[x] >: F[x], B, Z](fb: cats.Eval[F[B]])(f: (A, B) => Z)(implicit F: Do[F]): cats.Eval[F1[Z]] =
      F.!.map2Eval(fa, fb)(f)
    def flatMap[F1[x] >: F[x], B](f: A => F[B])(implicit F: Do[F1]): F1[B]             = F.!.flatMap(fa)(f)
    def productREval[F1[x] >: F[x], B](fb: cats.Eval[F[B]])(implicit F: Do[F1]): F1[B] = F.!.productREval(fa)(fb)
    def productLEval[F1[x] >: F[x], B](fb: cats.Eval[F[B]])(implicit F: Do[F1]): F1[A] = F.!.productLEval(fa)(fb)
    def mproduct[F1[x] >: F[x], B](f: A => F[B])(implicit F: Do[F1]): F1[(A, B)]       = F.!.mproduct(fa)(f)
    def flatTap[F1[x] >: F[x], B](f: A => F[B])(implicit F: Do[F1]): F1[A]             = F.!.flatTap(fa)(f)

    def flatten[F1[x] >: F[x], B](implicit ev: A <:< F1[B], F: Do[F1]): F1[B] = F.!.flatten(fa.asInstanceOf[F1[F1[B]]])

    def ap[F1[x] >: F[x], B, C](fb: F1[B])(implicit F: Do[F1], ev: A <:< (B => C)): F1[C] =
      F.!.ap(fa.asInstanceOf[F[B => C]])(fb)

    def <*>[F1[x] >: F[x], B, C](fb: F1[B])(implicit F: Do[F1], ev: A <:< (B => C)): F1[C] =
      F.!.ap(fa.asInstanceOf[F1[B => C]])(fb)

    def ap2[F1[x] >: F[x], B, C, D](fa: F1[B], fb: F1[C])(implicit F: Do[F1], ev: A <:< ((B, C) => D)): F1[D] =
      F.!.ap2(fa.asInstanceOf[F1[(B, C) => D]])(fa, fb)

  }

  final class TofuDoBooleanOps(private val condition: Boolean) extends AnyVal {
    def when_[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Unit] =
      if (condition) F.!.void(fa) else F.!.unit

    def unless_[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Unit] =
      if (condition) F.!.void(fa) else F.!.unit

    def whenOpt[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Option[A]] =
      if (condition) F.!.map(fa)(Some(_)) else F.!.pure(None)

    def unlessOpt[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Option[A]] =
      if (condition) F.!.pure(None) else F.!.map(fa)(Some(_))
  }

//  implicit final def tofuSyntaxIfM[F[_]: FlatMap](fa: F[Boolean]): IfMOps[F]             = new IfMOps[F](fa)
//  implicit final def tofuSyntaxFlatMapIdOps[A](a: A): FlatMapIdOps[A]                    = new FlatMapIdOps[A](a)
//  implicit final def tofuSyntaxFlatMapOps[F[_]: FlatMap, A](fa: F[A]): FlatMapOps[F, A]  = new FlatMapOps[F, A](fa)

}

trait DoSyntaxExtension extends DoSyntaxExtension1 {
  @inline final implicit def Do[F[_], A](fa: F[A]): DoMonad.TofuDoOps[F, A] = new DoMonad.TofuDoOps[F, A](fa)
}

trait DoSyntaxExtension1 {
//  final implicit def Do(b: Boolean): DoSyntax.TofuDoBooleanOps = new DoSyntax.TofuDoBooleanOps(b)
}

trait DoMonadInstances extends ScalaSpecificDoMonadInstances {
  import cats.instances.all._
  implicit val optionDoMonad: Do[Option]          = new DoMonad(catsStdInstancesForOption)
  implicit def eitherDoMonad[E]: Do[Either[E, *]] = new DoMonad(catsStdInstancesForEither[E])
  implicit val listDoMonad: Do[List]              = new DoMonad(catsStdInstancesForList)
  implicit val vectorDoMonad: Do[Vector]          = new DoMonad(catsStdInstancesForVector)
  implicit val queueDoMonad: Do[Queue]            = new DoMonad(catsStdInstancesForQueue)
  implicit def function1DoMonad[A]: Do[A => *]    = new DoMonad(catsStdMonadForFunction1[A])
  implicit val function0DoMonad: Do[() => *]      = new DoMonad(catsStdBimonadForFunction0)
}
