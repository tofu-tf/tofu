package tofu.internal
package carriers

trait ClockCE2CarrierMacro:
  inline given [F[_]]: ClockCE2Carrier[F] =
    Interop.delegate1[F, ClockCE2Carrier[F]]("tofu.interop.CE2Kernel.clock")

trait ClockCE3CarrierMacro:
  inline given [F[_]]: ClockCE3Carrier[F] =
    Interop.delegate1[F, ClockCE3Carrier[F]]("tofu.interop.CE3Kernel.clock")

trait SleepCE2CarrierMacro:
  inline given [F[_]]: SleepCE2Carrier[F] =
    Interop.delegate1[F, SleepCE2Carrier[F]]("tofu.interop.CE2Kernel.sleep")

trait SleepCE3CarrierMacro:
  inline given [F[_]]: SleepCE3Carrier[F] =
    Interop.delegate1[F, SleepCE3Carrier[F]]("tofu.interop.CE3Kernel.sleep")

trait TimeoutCE2CarrierMacro:
  inline given [F[_]]: TimeoutCE2Carrier[F] =
    Interop.delegate1[F, TimeoutCE2Carrier[F]]("tofu.interop.CE2Kernel.timeout")

trait TimeoutCE3CarrierMacro:
  inline given [F[_]]: TimeoutCE3Carrier[F] =
    Interop.delegate1[F, TimeoutCE3Carrier[F]]("tofu.interop.CE3Kernel.timeout")
