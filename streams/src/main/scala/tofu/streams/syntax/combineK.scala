package tofu.streams.syntax

import tofu.streams.CombineK

object combineK {

  implicit class LazyCombineKOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def repeat(implicit F: CombineK[F]): F[A] = F.combineK_(fa)(repeat)
  }
}
