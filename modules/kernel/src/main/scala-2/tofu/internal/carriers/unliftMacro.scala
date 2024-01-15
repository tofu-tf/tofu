package tofu.internal.carriers

import tofu.internal.Interop

trait UnliftCarrier2Macro {
  final implicit def unliftIOEffect[F[_], G[_]]: UnliftCarrier2[F, G] =
    macro Interop.delegate[UnliftCarrier2[F, G], G, { val `tofu.interop.CE2Kernel.unliftEffect`: Unit }]
}

trait UnliftCarrier3Macro {
  final implicit def unliftIOEffect[F[_], G[_]]: UnliftCarrier3[F, G] =
    macro Interop.delegate[UnliftCarrier3[F, G], G, { val `tofu.interop.CE3Kernel.unliftEffect`: Unit }]
}
