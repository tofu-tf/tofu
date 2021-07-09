package tofu.streams

import scala.concurrent.duration.FiniteDuration

trait Temporal[F[_], C[_]] {

  /** Divide `F` into groups of elements received within a time window,
    * or limited by the number of the elements, whichever happens first.
    */
  def groupWithin[A](fa: F[A])(n: Int, d: FiniteDuration): F[C[A]]
}

object Temporal {
  def apply[F[_], C[_]](implicit ev: Temporal[F, C]): Temporal[F, C] = ev
}
