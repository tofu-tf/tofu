package tofu

import monix.reactive.Observable

package object observable {
  implicit final class ObservableAdditionalOps[A](private val obs: Observable[A]) extends AnyVal {
    def takeWhileInclusive(p: A => Boolean): Observable[A] =
      obs.liftByOperator(TakeWhileInclusive[A](p, _))
  }
}
