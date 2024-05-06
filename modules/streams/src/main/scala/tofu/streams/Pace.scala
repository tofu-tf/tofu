package tofu.streams

import scala.concurrent.duration.FiniteDuration

trait Pace[F[_]] {

  /** Throttles `F` to the specified `rate`.
    */
  def throttled[A](fa: F[A])(rate: FiniteDuration): F[A]

  /** Delay pull from `F` for `d` duration.
    */
  def delay[A](fa: F[A])(d: FiniteDuration): F[A]
}

object Pace {
  def apply[F[_]](implicit ev: Pace[F]): Pace[F] = ev
}
