package tofu.time

import java.util.concurrent.TimeUnit

import tofu.internal.EffectComp
import tofu.internal.carriers._
import tofu.internal.instances.ClockInstance

trait Clock[F[_]] {

  /** Returns the current time, as a Unix timestamp (number of time units since the Unix epoch), suspended in `F[_]`.
    *
    * This is the pure equivalent of Java's `System.currentTimeMillis`, or of `CLOCK_REALTIME` from Linux's
    * `clock_gettime()`.
    */
  def realTime(unit: TimeUnit): F[Long]

  /** Returns a monotonic clock measurement, if supported by the underlying platform.
    *
    * This is the pure equivalent of Java's `System.nanoTime`, or of `CLOCK_MONOTONIC` from Linux's `clock_gettime()`.
    */
  def nanos: F[Long]
}

object Clock extends EffectComp[Clock] with ClockInstance
