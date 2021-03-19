package tofu.syntax
import cats.syntax._
import cats.{Applicative, Apply, FlatMap, Functor, Monad, Semigroupal}

object monadic extends TupleSemigroupalSyntax with ApplicativeSyntax with MonadSyntax {
  def unit[F[_]](implicit F: Applicative[F]): F[Unit] = F.unit

  implicit final class TofuFunctorOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Functor[F]): F[B]           = F.map(fa)(f)
    def fmap[B](f: A => B)(implicit F: Functor[F]): F[B]          = F.fmap(fa)(f)
    def widen[B >: A](implicit F: Functor[F]): F[B]               = F.widen(fa)
    def void(implicit F: Functor[F]): F[Unit]                     = F.void(fa)
    def fproduct[B](f: A => B)(implicit F: Functor[F]): F[(A, B)] = F.fproduct(fa)(f)
    def as[B](b: B)(implicit F: Functor[F]): F[B]                 = F.as(fa, b)
    def tupleLeft[B](b: B)(implicit F: Functor[F]): F[(B, A)]     = F.tupleLeft(fa, b)
    def tupleRight[B](b: B)(implicit F: Functor[F]): F[(A, B)]    = F.tupleRight(fa, b)
  }

  implicit final class TofuSemigroupalOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def product[B](fb: F[B])(implicit F: Semigroupal[F]): F[(A, B)] = F.product(fa, fb)
  }

  implicit final class TofuApplyFuncOps[F[_], A, B](private val fab: F[A => B]) extends AnyVal {
    def ap(fa: F[A])(implicit F: Apply[F]): F[B]  = F.ap(fab)(fa)
    def <*>(fa: F[A])(implicit F: Apply[F]): F[B] = F.ap(fab)(fa)
  }

  implicit final class TofuApplicativeBooleanOps(private val condition: Boolean) extends AnyVal {
    def when_[F[_], A](fa: => F[A])(implicit F: Applicative[F]): F[Unit] =
      if (condition) F.void(fa) else F.unit

    def unless_[F[_], A](fa: => F[A])(implicit F: Applicative[F]): F[Unit] =
      if (condition) F.unit else F.void(fa)

    def whenOpt[F[_], A](fa: => F[A])(implicit F: Applicative[F]): F[Option[A]] =
      if (condition) F.map(fa)(Some(_)) else F.pure(None)

    def unlessOpt[F[_], A](fa: => F[A])(implicit F: Applicative[F]): F[Option[A]] =
      if (condition) F.pure(None) else F.map(fa)(Some(_))
  }

  implicit final class TofuApplyFunc2Ops[F[_], A, B, C](private val fab: F[(A, B) => C]) extends AnyVal {
    def ap2(fa: F[A], fb: F[B])(implicit F: Apply[F]): F[C] = F.ap2(fab)(fa, fb)
  }

  implicit final class TofuApplyOps[F[_], C](private val fa: F[C]) extends AnyVal {
    def productR[B](fb: F[B])(implicit F: Apply[F]): F[B]                                          = F.productR(fa)(fb)
    def productL[B](fb: F[B])(implicit F: Apply[F]): F[C]                                          = F.productL(fa)(fb)
    def *>[B](fb: F[B])(implicit F: Apply[F]): F[B]                                                = F.productR(fa)(fb)
    def <*[B](fb: F[B])(implicit F: Apply[F]): F[C]                                                = F.productL(fa)(fb)
    def map2[B, Z](fb: F[B])(f: (C, B) => Z)(implicit F: Apply[F]): F[Z]                           = F.map2(fa, fb)(f)
    def map2Eval[B, Z](fb: cats.Eval[F[B]])(f: (C, B) => Z)(implicit F: Apply[F]): cats.Eval[F[Z]] =
      F.map2Eval(fa, fb)(f)
  }

  implicit final class TofuFlatMapOps[F[_], C](private val fa: F[C]) extends AnyVal {
    def flatMap[B](f: C => F[B])(implicit F: FlatMap[F]): F[B]             = F.flatMap(fa)(f)
    def productREval[B](fb: cats.Eval[F[B]])(implicit F: FlatMap[F]): F[B] = F.productREval(fa)(fb)
    def productLEval[B](fb: cats.Eval[F[B]])(implicit F: FlatMap[F]): F[C] = F.productLEval(fa)(fb)
    def mproduct[B](f: C => F[B])(implicit F: FlatMap[F]): F[(C, B)]       = F.mproduct(fa)(f)
    def flatTap[B](f: C => F[B])(implicit F: FlatMap[F]): F[C]             = F.flatTap(fa)(f)
    def replicateM_(n: Int)(implicit F: Monad[F]): F[Unit]                 =
      F.tailRecM(n)(k => if (k <= 0) F.pure(Right(())) else F.as(fa, Left(k - 1)))
  }

  implicit final def tofuSyntaxApplyOps[F[_], A](fa: F[A]): ApplyOps[F, A]               = new ApplyOps(fa)
  implicit final def tofuSyntaxFlatten[F[_]: FlatMap, A](ffa: F[F[A]]): FlattenOps[F, A] = new FlattenOps[F, A](ffa)
  implicit final def tofuSyntaxIfM[F[_]: FlatMap](fa: F[Boolean]): IfMOps[F]             = new IfMOps[F](fa)
  implicit final def tofuSyntaxFlatMapIdOps[A](a: A): FlatMapIdOps[A]                    = new FlatMapIdOps[A](a)
  implicit final def tofuSyntaxFlatMapOps[F[_]: FlatMap, A](fa: F[A]): FlatMapOps[F, A]  = new FlatMapOps[F, A](fa)
}
