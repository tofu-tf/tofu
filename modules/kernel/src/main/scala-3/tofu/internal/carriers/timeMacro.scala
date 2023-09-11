package tofu.internal
package carriers

trait ClockCE2CarrierMacro:
  implicit inline def interop[F[_]]: ClockCE2Carrier[F] =
    Interop.delegate1[F, ClockCE2Carrier[F]]("tofu.interop.CE2Kernel.clock")

trait ClockCE3CarrierMacro:
  implicit inline def interop[F[_]]: ClockCE3Carrier[F] =
    Interop.delegate1[F, ClockCE3Carrier[F]]("tofu.interop.CE3Kernel.clock")

trait SleepCE2CarrierMacro:
  implicit inline def interop[F[_]]: SleepCE2Carrier[F] =
    Interop.delegate1[F, SleepCE2Carrier[F]]("tofu.interop.CE2Kernel.sleep")

trait SleepCE3CarrierMacro:
  implicit inline def interop[F[_]]: SleepCE3Carrier[F] =
    Interop.delegate1[F, SleepCE3Carrier[F]]("tofu.interop.CE3Kernel.sleep")

trait TimeoutCE2CarrierMacro:
  implicit inline def interop[F[_]]: TimeoutCE2Carrier[F] =
    Interop.delegate1[F, TimeoutCE2Carrier[F]]("tofu.interop.CE2Kernel.timeout")

trait TimeoutCE3CarrierMacro:
  implicit inline def interop[F[_]]: TimeoutCE3Carrier[F] =
    Interop.delegate1[F, TimeoutCE3Carrier[F]]("tofu.interop.CE3Kernel.timeout")
