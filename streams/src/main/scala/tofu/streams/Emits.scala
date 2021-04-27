package tofu.streams

import cats.{Applicative, Foldable, Monad, MonoidK}
import cats.syntax.flatMap._
import tofu.lift.Lift

trait Emits[F[_]] {

  val monoidK: MonoidK[F]
  val applicative: Applicative[F]

  def emits[C[_], A](as: C[A])(implicit C: Foldable[C]): F[A] =
    C.foldLeft(as, monoidK.empty[A])((acc, a) => monoidK.combineK(acc, applicative.pure(a)))
}

object Emits {

  implicit def instance[F[_]: MonoidK: Applicative]: Emits[F] =
    new Emits[F] {
      override val monoidK: MonoidK[F]         = implicitly
      override val applicative: Applicative[F] = implicitly
    }

  def apply[F[_]](implicit ev: Emits[F]): Emits[F] = ev
}

trait Evals[F[_], G[_]] extends Emits[F] with Lift[G, F] {

  implicit val monad: Monad[F]

  lazy val applicative: Applicative[F] = monad

  def eval[A](ga: G[A]): F[A] = lift(ga)

  def evals[C[_]: Foldable, A](gsa: G[C[A]]): F[A] =
    eval(gsa) >>= (emits(_))

  def evalMap[A, B](fa: F[A])(f: A => G[B]): F[B] =
    fa >>= (a => eval(f(a)))
}

object Evals {

  implicit def instance[F[_]: Monad: MonoidK, G[_]](implicit lft: Lift[G, F]): Evals[F, G] =
    new Evals[F, G] {
      override val monad: Monad[F]     = implicitly
      override val monoidK: MonoidK[F] = implicitly

      override def lift[A](fa: G[A]): F[A] = lft.lift(fa)
    }

  def apply[F[_], G[_]](implicit ev: Evals[F, G]): Evals[F, G] = ev
}
