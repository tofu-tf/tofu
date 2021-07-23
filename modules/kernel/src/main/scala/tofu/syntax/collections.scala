package tofu.syntax

import cats.syntax._
import cats.{Applicative, Traverse, TraverseFilter}
import cats.syntax.functor._
import cats.data.State
import cats.data.StateT
import cats.Monad
import cats.free.Free
import cats.FlatMap
object collections
    extends FoldableSyntax with TraverseSyntax with TraverseFilterSyntax with FunctorFilterSyntax
    with TofuTraverseSyntax {

  final implicit class TofuCollectionsSyntax[F[_], A](private val fa: F[A]) extends AnyVal {

    /** a combination of map and scanLeft
      * it applies a function to each element of a structure,
      * passing an accumulating parameter from left to right,
      * and returning a final value of this accumulator together with the new structure.
      */
    def mapAccumL[B, C](fa: F[A], start: B)(step: (B, A) => (B, C))(implicit F: Traverse[F]): (B, F[C]) =
      F.traverse(fa)(a => State((b: B) => step(b, a))).run(start).value

    /** like mapAccumL, but drop the final state
      */
    def mapAccumL_[B, C](fa: F[A], start: B)(step: (B, A) => (B, C))(implicit F: Traverse[F]): F[C] =
      mapAccumL(fa, start)(step)._2

    /** like [[mapAccumL]] but also combines monadic effects of `G`
      * stack-safety relies on a stack safety of G
      */
    def mapAccumM[G[_]: Monad, B, C](fa: F[A], start: B)(step: (B, A) => G[(B, C)])(implicit
        F: Traverse[F]
    ): G[(B, F[C])] =
      F.traverse(fa)(a => StateT((b: B) => step(b, a))).run(start)

    /** like [[mapAccumM]] but drop the final state
      */
    def mapAccumM_[G[_]: Monad, B, C](fa: F[A], start: B)(step: (B, A) => G[(B, C)])(implicit
        F: Traverse[F]
    ): G[F[C]] = mapAccumM(fa, start)(step).map(_._2)

    /** like [[mapAccumL]] but also combines monadic effects of `G`
      * stack-safety guaranteed via Free
      */
    def mapAccumF[G[_]: Monad, B, C](fa: F[A], start: B)(step: (B, A) => G[(B, C)])(implicit
        F: Traverse[F]
    ): G[(B, F[C])] =
      F.traverse(fa)(a => StateT((b: B) => Free.liftF(step(b, a)))).run(start).runTailRec

    /** like [[mapAccumF]] but drop the final state
      */
    def mapAccumF_[G[_]: Monad, B, C](fa: F[A], start: B)(step: (B, A) => G[(B, C)])(implicit
        F: Traverse[F]
    ): G[F[C]] = mapAccumF(fa, start)(step).map(_._2)
  }

  final implicit class TofuSequenceOps[G[_], T[_], A](private val fta: T[G[A]]) extends AnyVal {
    def sequence(implicit G: Applicative[G], T: Traverse[T]): G[T[A]] =
      T.sequence[G, A](fta)
  }

  final implicit class TofuFlatSequenceOps[G[_], T[_], A](private val fta: T[G[T[A]]]) extends AnyVal {
    def flatSequence(implicit
        G: Applicative[G],
        T: Traverse[T],
        TF: FlatMap[T],
    ): G[T[A]] = T.flatSequence[G, A](fta)
  }
}

@deprecated("use tofu.syntax.collections", since = "0.11.0")
object traverse extends TofuTraverseSyntax

trait TofuTraverseSyntax {
  final implicit def tofuTraverseSyntax[F[_], A](ta: F[A]): TraverseOps[F, A] = new TraverseOps[F, A](ta)
}

final class TraverseOps[F[_], A](private val ta: F[A]) extends AnyVal {
  def traverseKey[G[_]: Applicative, B](f: A => G[B])(implicit T: Traverse[F]): G[F[(A, B)]] =
    T.traverse(ta)(a => f(a).map((a, _)))

  def traverseKeyFilter[G[_]: Applicative, B](f: A => G[Option[B]])(implicit TF: TraverseFilter[F]): G[F[(A, B)]] =
    TF.traverseFilter(ta)(a => f(a).map(_.map((a, _))))
}
