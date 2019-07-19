package tofu.syntax

import cats.Functor
import cats.effect.concurrent.Ref
import tofu.concurrent.FocusedRef
import tofu.optics.Contains

object concurrent {
  implicit class FPUtilsRefOps[F[_], A](val ref: Ref[F, A]) extends AnyVal {
    def focused[B](implicit focus: A Contains B, F: Functor[F]): Ref[F, B] = FocusedRef(ref, focus)
  }
}
