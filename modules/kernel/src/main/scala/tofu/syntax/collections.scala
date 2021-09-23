package tofu.syntax

import cats.data.{State, StateT}
import cats.free.Free
import cats.syntax._
import cats.syntax.functor._
import cats.{Applicative, FlatMap, Foldable, Monad, Traverse, TraverseFilter}
import tofu.internal.FoldableStream

object collections
    extends FoldableSyntax with TraverseFilterSyntax with FunctorFilterSyntax with TofuTraverseSyntax
    with TofuFoldableSyntax {

  final implicit class CatsTraverseSyntax[F[_], A](private val self: F[A]) extends AnyVal {
    def traverse[G[_]: Applicative, B](f: A => G[B])(implicit FT: Traverse[F]): G[F[B]] =
      FT.traverse[G, A, B](self)(f)

    def traverseTap[G[_]: Applicative, B](f: A => G[B])(implicit FT: Traverse[F]): G[F[A]] =
      FT.traverseTap[G, A, B](self)(f)

    def flatTraverse[G[_]: Applicative, B](f: A => G[F[B]])(implicit F: FlatMap[F], FT: Traverse[F]): G[F[B]] =
      FT.flatTraverse[G, A, B](self)(f)

    def mapWithIndex[B](f: (A, Int) => B)(implicit FT: Traverse[F]): F[B] =
      FT.mapWithIndex[A, B](self)(f)

    def traverseWithIndexM[G[_]: Monad, B](f: (A, Int) => G[B])(implicit FT: Traverse[F]): G[F[B]] =
      FT.traverseWithIndexM[G, A, B](self)(f)

    def zipWithIndex(implicit FT: Traverse[F]): F[(A, Int)] = FT.zipWithIndex[A](self)
  }

  final implicit class TofuCollectionsSyntax[F[_], A](private val fa: F[A]) extends AnyVal {

    /** a combination of map and scanLeft it applies a function to each element of a structure, passing an accumulating
      * parameter from left to right, and returning a final value of this accumulator together with the new structure.
      */
    def mapAccumL[B, C](start: B)(step: (B, A) => (B, C))(implicit F: Traverse[F]): (B, F[C]) =
      F.traverse(fa)(a => State((b: B) => step(b, a))).run(start).value

    /** like mapAccumL, but drop the final state
      */
    def mapAccumL_[B, C](start: B)(step: (B, A) => (B, C))(implicit F: Traverse[F]): F[C] =
      mapAccumL(start)(step)._2

    /** accumulate values, producing intermediate results initial state will posess a first item place in the traverse
      * order final state is returned as a separate result
      */
    def scanL[B](start: B)(step: (B, A) => B)(implicit F: Traverse[F]): (B, F[B]) =
      mapAccumL(start)((b, a) => (b, step(b, a)))

    /** like [[mapAccumL]] but also combines monadic effects of `G` stack-safety relies on a stack safety of G
      */
    def mapAccumM[G[_]: Monad, B, C](start: B)(step: (B, A) => G[(B, C)])(implicit F: Traverse[F]): G[(B, F[C])] =
      F.traverse(fa)(a => StateT((b: B) => step(b, a))).run(start)

    /** like [[mapAccumM]] but drop the final state
      */
    def mapAccumM_[G[_]: Monad, B, C](start: B)(step: (B, A) => G[(B, C)])(implicit F: Traverse[F]): G[F[C]] =
      mapAccumM(start)(step).map(_._2)

    /** like [[mapAccumL]] but also combines monadic effects of `G` stack-safety guaranteed via Free
      */
    def mapAccumF[G[_]: Monad, B, C](start: B)(step: (B, A) => G[(B, C)])(implicit F: Traverse[F]): G[(B, F[C])] =
      F.traverse(fa)(a => StateT((b: B) => Free.liftF(step(b, a)))).run(start).runTailRec

    /** like [[mapAccumF]] but drop the final state
      */
    def mapAccumF_[G[_]: Monad, B, C](start: B)(step: (B, A) => G[(B, C)])(implicit F: Traverse[F]): G[F[C]] =
      mapAccumF(start)(step).map(_._2)

    /** accumulate values effectfully, producing intermediate results initial state will posess a first item place in
      * the traverse order final state is returned as a separate result
      */
    def scanF[G[_]: Monad, B](start: B)(step: (B, A) => G[B])(implicit F: Traverse[F]): G[(B, F[B])] =
      mapAccumF(start)((b, a) => step(b, a).tupleLeft(b))
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

@deprecated("use tofu.syntax.collections", since = "0.10.4")
object traverse extends TofuTraverseSyntax

trait TofuTraverseSyntax {
  final implicit def tofuTraverseSyntax[F[_], A](ta: F[A]): TraverseOps[F, A] = new TraverseOps[F, A](ta)
}

@deprecated("use tofu.syntax.collections", since = "0.10.4")
object foldable extends TofuFoldableSyntax

final class TraverseOps[F[_], A](private val ta: F[A]) extends AnyVal {
  def traverseKey[G[_]: Applicative, B](f: A => G[B])(implicit T: Traverse[F]): G[F[(A, B)]] =
    T.traverse(ta)(a => f(a).map((a, _)))

  def traverseKeyFilter[G[_]: Applicative, B](f: A => G[Option[B]])(implicit TF: TraverseFilter[F]): G[F[(A, B)]] =
    TF.traverseFilter(ta)(a => f(a).map(_.map((a, _))))
}

trait TofuFoldableSyntax {
  final implicit def tofuFoldableSyntax[F[_], A](ta: F[A]): TofuFoldableOps[F, A] = new TofuFoldableOps[F, A](ta)
}

final class TofuFoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {

  /** Applies monadic transfomation, feeding source collection, until operation results in None or collection is
    * consumed
    *
    * @param initial
    *   initial state
    * @param f
    *   state transformation, None would not be continued
    * @return
    *   final achieved state or initial
    */
  def foldWhileM[G[_], S](initial: S)(f: (S, A) => G[Option[S]])(implicit F: Foldable[F], G: Monad[G]): G[S] =
    G.tailRecM((initial, FoldableStream.from(fa))) {
      case (s, FoldableStream.Empty)      => G.pure(Right(s))
      case (s, FoldableStream.Cons(h, t)) =>
        G.map(f(s, h)) {
          case None     => Right(s)
          case Some(s1) => Left((s1, t.value))
        }
    }

  /** transforms each element to another type using monadic transformation until it resutls in None
    *
    * @param f
    *   element transformation, None would not be continued
    * @return
    *   a collection of transformed elements
    */
  def takeWhileM[G[_], B](f: A => G[Option[B]])(implicit F: Foldable[F], G: Monad[G]): G[List[B]] =
    G.map(foldWhileM(List.empty[B])((acc, a) => G.map(f(a))(_.map(acc.::))))(_.reverse)
}
