package tofu.time

import tofu.internal.EffectComp
import tofu.internal.carriers.{SleepCE2Carrier, SleepCE3Carrier}

import scala.concurrent.duration.FiniteDuration

trait Sleep[F[_]] {

  /** Pauses execution for desired duration
    */
  def sleep(duration: FiniteDuration): F[Unit]
}

object Sleep extends EffectComp[Sleep] with SleepInterop

trait SleepInterop extends SleepInterop1 {
  implicit def ce3Interop[F[_]](implicit sleep: SleepCE3Carrier[F]): Sleep[F] = sleep
}

trait SleepInterop1 {
  implicit def ce2Interop[F[_]](implicit sleep: SleepCE2Carrier[F]): Sleep[F] = sleep
}
