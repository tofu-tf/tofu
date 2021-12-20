package tofu.syntax

import cats.arrow.Category

object optics {
  // version of cats.syntax.compose with subtyping support
  implicit final class OpticOps[T[_, _], A, B](private val tab: T[A, B]) extends AnyVal {
    def >>>[Q[x, y] >: T[x, y], C](qbc: Q[B, C])(implicit cat: Category[Q]): Q[A, C] =
      cat.compose(qbc, tab)

    def andThen[Q[x, y] >: T[x, y], C](qbc: Q[B, C])(implicit cat: Category[Q]): Q[A, C] =
      cat.compose(qbc, tab)

    def <<<[Q[x, y] >: T[x, y], C](qbc: Q[C, A])(implicit cat: Category[Q]): Q[C, B] =
      cat.compose(tab, qbc)

    def compose[Q[x, y] >: T[x, y], C](qbc: Q[C, A])(implicit cat: Category[Q]): Q[C, B] =
      cat.compose(tab, qbc)
  }
}
