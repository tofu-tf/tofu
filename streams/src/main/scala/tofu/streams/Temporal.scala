package tofu.streams

import scala.concurrent.duration.FiniteDuration

trait Temporal[F[_]] {

  /** Throttles the stream to the specified `rate`.
    */
  def metered[A](fa: F[A])(rate: FiniteDuration): F[A]

  /** Delay pull from stream for `d` duration.
    */
  def delay[A](fa: F[A])(d: FiniteDuration): F[A]
}
