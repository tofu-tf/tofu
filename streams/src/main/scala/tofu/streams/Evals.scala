package tofu.streams

import cats.{Applicative, Foldable, Monad}

trait Emits[F[_]] {

  val applicative: Applicative[F]

  final def emit[A](a: A): F[A] = applicative.pure(a)

  def emits[C[_]: Foldable, A](as: C[A]): F[A]
}

trait Evals[F[_], G[_]] extends Emits[F] {

  val monad: Monad[F]

  override val applicative: Applicative[F] = monad

  def eval[A](ga: G[A]): F[A]

  def evals[C[_]: Foldable, A](gsa: G[C[A]]): F[A] =
    monad.flatMap(eval(gsa))(emits(_))

  final def evalMap[A, B](fa: F[A])(f: A => G[B]): F[B] =
    monad.flatMap(fa)(a => eval(f(a)))
}
