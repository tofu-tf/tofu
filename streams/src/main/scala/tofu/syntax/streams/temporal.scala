package tofu.syntax.streams

import tofu.streams.Temporal

import scala.concurrent.duration.FiniteDuration

object temporal {

  implicit final class TemporalOps[F[_], C[_], A](fa: F[A])(implicit tmp: Temporal[F, C]) {
    def groupWithin(n: Int, d: FiniteDuration): F[C[A]] = tmp.groupWithin(fa)(n, d)
  }
}
