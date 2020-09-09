package tofu.alias

import cats.{Alternative, Monad}
import tofu.compat.unused212

class DoMonad[F[_]](val ! : Monad[F]) extends AnyVal

object DoMonad {
  final implicit def doMonad[F[_]](implicit F: Monad[F]): Do[F] = new DoMonad[F](F)

  final class TofuDoOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Do[F]): F[B]                                                              = F.!.map(fa)(f)
    def fmap[B](f: A => B)(implicit F: Do[F]): F[B]                                                             = F.!.fmap(fa)(f)
    def widen[B >: A](implicit F: Do[F]): F[B]                                                                  = F.!.widen(fa)
    def void(implicit F: Do[F]): F[Unit]                                                                        = F.!.void(fa)
    def fproduct[B](f: A => B)(implicit F: Do[F]): F[(A, B)]                                                    = F.!.fproduct(fa)(f)
    def as[B](b: B)(implicit F: Do[F]): F[B]                                                                    = F.!.as(fa, b)
    def tupleLeft[B](b: B)(implicit F: Do[F]): F[(B, A)]                                                        = F.!.tupleLeft(fa, b)
    def tupleRight[B](b: B)(implicit F: Do[F]): F[(A, B)]                                                       = F.!.tupleRight(fa, b)
    def product[B](fb: F[B])(implicit F: Do[F]): F[(A, B)]                                                      = F.!.product(fa, fb)
    def productR[F1[x] >: F[x], B](fb: F1[B])(implicit F: Do[F1]): F1[B]                                        = F.!.productR(fa)(fb)
    def productL[F1[x] >: F[x], B](fb: F1[B])(implicit F: Do[F1]): F1[A]                                        = F.!.productL(fa)(fb)
    def *>[F1[x] >: F[x], B](fb: F1[B])(implicit F: Do[F1]): F1[B]                                              = F.!.productR(fa)(fb)
    def <<[F1[x] >: F[x], B](fb: => F[B])(implicit F: Do[F1]): F1[A]                                            = F.!.flatMap(fa)(a => F.!.as(fb, a))
    def <*[F1[x] >: F[x], B](fb: F[B])(implicit F: Do[F1]): F1[A]                                               = F.!.productL(fa)(fb)
    def >>[F1[x] >: F[x], B](fb: => F[B])(implicit F: Do[F1]): F1[B]                                            = F.!.flatMap(fa)(_ => fb)
    def map2[F1[x] >: F[x], B, Z](fb: F[B])(f: (A, B) => Z)(implicit F: Do[F1]): F1[Z]                          = F.!.map2(fa, fb)(f)
    def map2Eval[F1[x] >: F[x], B, Z](fb: cats.Eval[F[B]])(f: (A, B) => Z)(implicit F: Do[F]): cats.Eval[F1[Z]] =
      F.!.map2Eval(fa, fb)(f)

    def flatMap[F1[x] >: F[x], B](f: A => F1[B])(implicit F: Do[F1]): F1[B] = F.!.flatMap(fa)(f)
    def >>=[F1[x] >: F[x], B](f: A => F1[B])(implicit F: Do[F1]): F1[B]     = F.!.flatMap(fa)(f)

    def foreverM[B](implicit F: Do[F]): F[B] = F.!.foreverM[A, B](fa)

    def productREval[F1[x] >: F[x], B](fb: cats.Eval[F1[B]])(implicit F: Do[F1]): F1[B] = F.!.productREval(fa)(fb)
    def productLEval[F1[x] >: F[x], B](fb: cats.Eval[F1[B]])(implicit F: Do[F1]): F1[A] = F.!.productLEval(fa)(fb)
    def mproduct[F1[x] >: F[x], B](f: A => F1[B])(implicit F: Do[F1]): F1[(A, B)]       = F.!.mproduct(fa)(f)
    def flatTap[F1[x] >: F[x], B](f: A => F1[B])(implicit F: Do[F1]): F1[A]             = F.!.flatTap(fa)(f)

    def flatten[F1[x] >: F[x], B](implicit @unused212 ev: A <:< F1[B], F: Do[F1]): F1[B] =
      F.!.flatten(fa.asInstanceOf[F1[F1[B]]])

    def ap[F1[x] >: F[x], B, C](fb: F1[B])(implicit F: Do[F1], @unused212 ev: A <:< (B => C)): F1[C] =
      F.!.ap(fa.asInstanceOf[F[B => C]])(fb)

    def <*>[F1[x] >: F[x], B, C](fb: F1[B])(implicit F: Do[F1], @unused212 ev: A <:< (B => C)): F1[C] =
      F.!.ap(fa.asInstanceOf[F1[B => C]])(fb)

    def ap2[F1[x] >: F[x], B, C, D](fa: F1[B], fb: F1[C])(implicit
        F: Do[F1],
        @unused212 ev: A <:< ((B, C) => D)
    ): F1[D] =
      F.!.ap2(fa.asInstanceOf[F1[(B, C) => D]])(fa, fb)

    def whenM[F1[x] >: F[x], B](fb: => F1[B])(implicit F: Do[F1], @unused212 ev: A <:< Boolean): F1[Unit] =
      F.!.flatMap(fa)(a => if (ev(a)) F.!.void(fb) else F.!.unit)

    def unlessM[F1[x] >: F[x], B](fb: => F1[B])(implicit F: Do[F1], @unused212 ev: A <:< Boolean): F1[Unit] =
      F.!.flatMap(fa)(a => if (ev(a)) F.!.unit else F.!.void(fb))

    def ifM[F1[x] >: F[x], B](fthen: => F1[B], felse: => F1[B])(implicit
        F: Do[F1],
        @unused212 ev: A <:< Boolean
    ): F1[B] =
      F.!.ifM(fa.asInstanceOf[F[Boolean]])(fthen, felse)

    def iterateWhile(f: A => Boolean)(implicit F: Do[F]): F[A] = F.!.iterateWhile(fa)(f)

    def iterateUntil(f: A => Boolean)(implicit F: Do[F]): F[A] = F.!.iterateUntil(fa)(f)

    def untilM[G[_]](fb: => F[Boolean])(implicit F: Do[F], G: Alternative[G]): F[G[A]] = F.!.untilM(fa)(fb)

    def untilM_[F1[x] >: F[x]](fb: => F1[Boolean])(implicit F: Do[F1]): F1[Unit] = F.!.untilM_(fa)(fb)
  }

  class DoMethods[F[_]](private val __ : Boolean) extends AnyVal {
    def pure[A](a: A)(implicit F: Do[F]): F[A] = F.!.pure(a)
    def unit(implicit F: Do[F]): F[Unit]       = F.!.unit

    def ~[F1[_], A](fa: => F1[A]) = new DoTofuByNameOps[F1, A](() => fa)
    def loop[A](a: A)             = new DoTofuLoopOps[A](a)
  }

  class DoTofuByNameOps[F[_], A](private val fa: () => F[A]) extends AnyVal {
    def when(condition: Boolean)(implicit F: Do[F]): F[Unit] =
      if (condition) F.!.void(fa()) else F.!.unit

    def unless(condition: Boolean)(implicit F: Do[F]): F[Unit] =
      if (condition) F.!.void(fa()) else F.!.unit

    def whenOpt(condition: Boolean)(implicit F: Do[F]): F[Option[A]] =
      if (condition) F.!.map(fa())(Some(_)) else F.!.pure(None)

    def unlessOpt(condition: Boolean)(implicit F: Do[F]): F[Option[A]] =
      if (condition) F.!.pure(None) else F.!.map(fa())(Some(_))

    def whenM[F1[x] >: F[x]](fb: F1[Boolean])(implicit F: Do[F1]): F1[Unit] =
      F.!.flatMap(fb)(if (_) F.!.void(fa()) else F.!.unit)

    def unlessM[F1[x] >: F[x]](fb: F1[Boolean])(implicit F: Do[F1]): F1[Unit] =
      F.!.flatMap(fb)(if (_) F.!.unit else F.!.void(fa()))

    def whenOptM[F1[x] >: F[x]](fb: F1[Boolean])(implicit F: Do[F1]): F1[Option[A]] =
      F.!.flatMap(fb)(if (_) F.!.map(fa())(Some(_)) else F.!.pure(None))

    def unlessOptM[F1[x] >: F[x]](fb: F1[Boolean])(implicit F: Do[F1]): F1[Option[A]] =
      F.!.flatMap(fb)(if (_) F.!.pure(None) else F.!.map(fa())(Some(_)))

    def whileM_[F1[x] >: F[x]](fb: F[Boolean])(implicit F: Do[F]): F[Unit] = F.!.whileM_(fb)(fa())

    def whileM[G[_]](fb: F[Boolean])(implicit F: Do[F], G: Alternative[G]): F[G[A]] = F.!.whileM(fb)(fa())
  }

  class DoTofuLoopOps[A](private val a: A) extends AnyVal {
    def iterate[F[_]](f: A => F[A]): DoTofuIterateOps[F, A]                 = new DoTofuIterateOps[F, A](a, f)
    def tailRecM[F[_], B](f: A => F[Either[A, B]])(implicit F: Do[F]): F[B] = F.!.tailRecM(a)(f)
  }

  class DoTofuIterateOps[F[_], A](a: A, fa: A => F[A]) {
    def whileM(f: A => Boolean)(implicit F: Do[F]): F[A] = F.!.iterateWhileM(a)(fa)(f)
    def untilM(f: A => Boolean)(implicit F: Do[F]): F[A] = F.!.iterateUntilM(a)(fa)(f)
  }

  class DoBooleanMethods(private val condition: Boolean) extends AnyVal {
    def when[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Unit] =
      if (condition) F.!.void(fa) else F.!.unit

    def unless[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Unit] =
      if (condition) F.!.void(fa) else F.!.unit

    def whenOpt[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Option[A]] =
      if (condition) F.!.map(fa)(Some(_)) else F.!.pure(None)

    def unlessOpt[F[_], A](fa: => F[A])(implicit F: Do[F]): F[Option[A]] =
      if (condition) F.!.pure(None) else F.!.map(fa)(Some(_))
  }
}

trait DoSyntaxExtension extends DoSyntaxExtension1 {
  def Do(b: Boolean): DoMonad.DoBooleanMethods = new DoMonad.DoBooleanMethods(b)
}

trait DoSyntaxExtension1 {
  def Do[F[_]] = new DoMonad.DoMethods[F](true)

  @inline final implicit def Do[F[_], A](fa: F[A]): DoMonad.TofuDoOps[F, A] = new DoMonad.TofuDoOps[F, A](fa)
}
