package tofu.streams

import cats.{Alternative, Foldable, Monad}
import cats.syntax.applicative._
import cats.syntax.flatMap._

trait Evals[F[_], G[_]] {

  implicit val monad: Monad[F]

  val alternative: Alternative[F]

  def eval[A](ga: G[A]): F[A]

  final def evals[C[_]: Foldable, A](gsa: G[C[A]]): F[A] =
    eval(gsa) >>= (ca => alternative.unite(ca.pure))

  final def evalMap[A, B](fa: F[A])(f: A => G[B]): F[B] =
    fa >>= (a => eval(f(a)))
}
