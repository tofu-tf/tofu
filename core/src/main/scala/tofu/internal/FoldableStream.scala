package tofu.internal

import cats.Foldable
import cats.Eval

sealed trait FoldableStream[+A]

object FoldableStream {
  def from[F[_], A](fa: F[A])(implicit F: Foldable[F]): FoldableStream[A] =
    F.foldRight(fa, emptyE[A])((h, t) => Eval.always(Cons(h, t))).value

  val emptyNothing: Eval[FoldableStream[Nothing]] = Eval.now(Empty)
  def emptyE[A]: Eval[FoldableStream[A]]          = emptyNothing
  case object Empty                                              extends FoldableStream[Nothing]
  final case class Cons[+A](a: A, tail: Eval[FoldableStream[A]]) extends FoldableStream[A]
}
