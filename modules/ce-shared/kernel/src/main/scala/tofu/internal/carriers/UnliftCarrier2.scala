package tofu.internal.carriers

import tofu.internal.Interop
import tofu.lift.Unlift

// This is purely workaround for scala 2.12
// Which denies to unfold a macro (and recieve a type error)
// before checking an implicit for eligibility
trait UnliftCarrier2[F[_], G[_]] extends Unlift[F, G]

object UnliftCarrier2 {
  final implicit def unliftIOEffect[F[_], G[_]]: UnliftCarrier2[F, G] =
    macro Interop.delegate[UnliftCarrier2[F, G], G, { val `tofu.interop.CE2Kernel.unliftEffect`: Unit }]
}

trait UnliftCarrier3[F[_], G[_]] extends Unlift[F, G]

object UnliftCarrier3 {
  final implicit def unliftIOEffect[F[_], G[_]]: UnliftCarrier3[F, G] =
    macro Interop.delegate[UnliftCarrier3[F, G], G, { val `tofu.interop.CE3Kernel.unliftEffect`: Unit }]
}
