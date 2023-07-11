package tofu.internal
package carriers

trait DelayCarrier2Macro {
  final implicit def interop[F[_]]: DelayCarrier2[F] =
    macro Interop.delegate[DelayCarrier2[F], F, { val `tofu.interop.CE2Kernel.delayViaSync`: Unit }]
}

trait DelayCarrier3Macro {
  final implicit def interop[F[_]]: DelayCarrier3[F] =
    macro Interop.delegate[DelayCarrier3[F], F, { val `tofu.interop.CE3Kernel.delayViaSync`: Unit }]
}
