package tofu.internal
package carriers

trait ClockCE2CarrierMacro {
  final implicit def interop[F[_]]: ClockCE2Carrier[F] =
    macro Interop.delegate[ClockCE2Carrier[F], F, { val `tofu.interop.CE2Kernel.clock`: Unit }]
}

trait ClockCE3CarrierMacro {
  final implicit def interop[F[_]]: ClockCE3Carrier[F] =
    macro Interop.delegate[ClockCE3Carrier[F], F, { val `tofu.interop.CE3Kernel.clock`: Unit }]
}

trait SleepCE2CarrierMacro {
  final implicit def interop[F[_]]: SleepCE2Carrier[F] =
    macro Interop.delegate[SleepCE2Carrier[F], F, { val `tofu.interop.CE2Kernel.sleep`: Unit }]
}

trait SleepCE3CarrierMacro {
  final implicit def interop[F[_]]: SleepCE3Carrier[F] =
    macro Interop.delegate[SleepCE3Carrier[F], F, { val `tofu.interop.CE3Kernel.sleep`: Unit }]
}

trait TimeoutCE2CarrierMacro {
  final implicit def interop[F[_]]: TimeoutCE2Carrier[F] =
    macro Interop.delegate[TimeoutCE2Carrier[F], F, { val `tofu.interop.CE2Kernel.timeout`: Unit }]
}

trait TimeoutCE3CarrierMacro {
  final implicit def interop[F[_]]: TimeoutCE3Carrier[F] =
    macro Interop.delegate[TimeoutCE3Carrier[F], F, { val `tofu.interop.CE3Kernel.timeout`: Unit }]
}