package tofu.streams.syntax

import tofu.streams.Evals

object evals {

  implicit final class EvalsOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
    def evalMap[B](f: A => G[B])(implicit evals: Evals[F, G]): F[B] = evals.evalMap(fa)(f)
  }
}
