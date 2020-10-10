package tofu.syntax.streams

import tofu.streams.Pace

import scala.concurrent.duration.FiniteDuration

private[syntax] final class PaceOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def throttled(rate: FiniteDuration)(implicit ev: Pace[F]): F[A] = ev.throttled(fa)(rate)
  def delay(d: FiniteDuration)(implicit ev: Pace[F]): F[A]        = ev.delay(fa)(d)
}

private[syntax] trait PaceSyntax {
  implicit def toPaceOps[F[_], A](fa: F[A]): PaceOps[F, A] = new PaceOps(fa)
}
