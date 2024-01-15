package tofu.internal
package instances

import tofu.internal.carriers.{UnliftCarrier2, UnliftCarrier3}
import tofu.lift.Unlift

private[tofu] trait ContextBaseCarrierInstance extends ContextBaseCarrierInstance2 {
  final implicit def unliftEffectCE3[F[_], G[_]](implicit carrier: UnliftCarrier3[F, G]): Unlift[F, G] =
    carrier
}

private[tofu] trait ContextBaseCarrierInstance2 {
  final implicit def unliftEffectCE2[F[_], G[_]](implicit carrier: UnliftCarrier2[F, G]): Unlift[F, G] =
    carrier
}
