package tofu.streams.syntax

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{FlatMap, Foldable}
import tofu.lift.Lift
import tofu.streams.Emits

object evals {

  def eval[F[_]]: EvalPA[F] = new EvalPA[F](true)

  private[syntax] final class EvalPA[F[_]](private val __ : Boolean) extends AnyVal {
    def apply[G[_], A](ga: G[A])(implicit ev: Lift[G, F]): F[A] = ev.lift(ga)
  }

  def evals[F[_]]: EvalsPA[F] = new EvalsPA[F](true)

  private[syntax] final class EvalsPA[F[_]](private val __ : Boolean) extends AnyVal {
    def apply[G[_], C[_]: Foldable, A](gca: G[C[A]])(implicit lift: Lift[G, F], emits: Emits[F], F: FlatMap[F]): F[A] =
      eval(gca) >>= (emits.emits(_))
  }

  implicit final class EvalsOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
    def evalMap[B](f: A => G[B])(implicit lift: Lift[G, F], F: FlatMap[F]): F[B] = fa >>= (a => eval(f(a)))
    def evalTap[B](f: A => G[B])(implicit lift: Lift[G, F], F: FlatMap[F]): F[A] = fa >>= (a => eval(f(a)) as a)
  }
}
