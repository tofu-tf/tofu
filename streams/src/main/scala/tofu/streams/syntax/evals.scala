package tofu.streams.syntax

import tofu.streams.Evals

object evals {

  implicit final class EvalsOps[F[_], G[_], A](fa: F[A])(implicit evals: Evals[F, G]) {
    def evalMap[B](f: A => G[B]): F[B] = evals.evalMap(fa)(f)
  }
}
