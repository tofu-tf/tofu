package tofu.internal
package carriers

import tofu.time.{Clock, Sleep}
import tofu.time.Timeout

trait ClockCE2Carrier[F[_]] extends Clock[F]

object ClockCE2Carrier extends ClockCE2CarrierMacro

trait ClockCE3Carrier[F[_]] extends Clock[F]

object ClockCE3Carrier extends ClockCE3CarrierMacro

trait SleepCE2Carrier[F[_]] extends Sleep[F]

object SleepCE2Carrier extends SleepCE2CarrierMacro

trait SleepCE3Carrier[F[_]] extends Sleep[F]

object SleepCE3Carrier extends SleepCE3CarrierMacro

trait TimeoutCE2Carrier[F[_]] extends Timeout[F]

object TimeoutCE2Carrier extends TimeoutCE2CarrierMacro

trait TimeoutCE3Carrier[F[_]] extends Timeout[F]

object TimeoutCE3Carrier extends TimeoutCE3CarrierMacro
