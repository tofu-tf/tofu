package tofu.syntax.streams

import tofu.streams.Pace

import scala.concurrent.duration.FiniteDuration

object pace {

  implicit final class PaceOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def throttled(rate: FiniteDuration)(implicit ev: Pace[F]): F[A] = ev.throttled(fa)(rate)
    def delay(d: FiniteDuration)(implicit ev: Pace[F]): F[A]        = ev.delay(fa)(d)
  }
}
