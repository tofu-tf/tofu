package tofu.internal
package carriers

import tofu.Delay

trait DelayCarrier2[F[_]] extends Delay[F]

object DelayCarrier2 {
  final implicit def interop[F[_]]: DelayCarrier2[F] =
    macro Interop.delegate[DelayCarrier2[F], F, { val `tofu.interop.CE2Kernel.delayViaSync`: Unit }]
}

trait DelayCarrier3[F[_]] extends Delay[F]

object DelayCarrier3 {
  final implicit def interop[F[_]]: DelayCarrier3[F] =
    macro Interop.delegate[DelayCarrier3[F], F, { val `tofu.interop.CE3Kernel.delayViaSync`: Unit }]
}
