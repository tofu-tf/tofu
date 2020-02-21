package tofu.syntax

import cats.Foldable
import cats.Monad
import tofu.internal.FoldableStream

object foldable {
  final implicit class TofuFoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {

    /**
      * Applies monadic transfomation, feeding source collection,
      * until operation results in None or collection is consumed
      *
      * @param initial initial state
      * @param f state transformation, None would not be continued
      * @return final achieved state or initial
      */
    def foldWhileM[G[_], S](initial: S)(f: (S, A) => G[Option[S]])(implicit F: Foldable[F], G: Monad[G]): G[S] =
      G.tailRecM((initial, FoldableStream.from(fa))) {
        case (s, FoldableStream.Empty) => G.pure(Right(s))
        case (s, FoldableStream.Cons(h, t)) =>
          G.map(f(s, h)) {
            case None     => Right(s)
            case Some(s1) => Left((s1, t.value))
          }
      }

    /**
      * transforms each element to another type using monadic transformation
      * until it resutls in None
      *
      * @param f element transformation, None would not be continued
      * @return a collection of transformed elements
      */
    def takeWhileM[G[_], B](f: A => G[Option[B]])(implicit F: Foldable[F], G: Monad[G]): G[List[B]] =
      G.map(foldWhileM(List.empty[B])((acc, a) => G.map(f(a))(_.map(acc.::))))(_.reverse)
  }
}
