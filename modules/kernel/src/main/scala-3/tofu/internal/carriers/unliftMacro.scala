package tofu.internal.carriers

import tofu.internal.Interop

trait UnliftCarrier2Macro:
  inline given [F[_], G[_]]: UnliftCarrier2[F, G] =
    Interop.delegate1[G, UnliftCarrier2[F, G]]("tofu.interop.CE2Kernel.unliftEffect")

trait UnliftCarrier3Macro:
  inline given [F[_], G[_]]: UnliftCarrier3[F, G] =
    Interop.delegate1[G, UnliftCarrier3[F, G]]("tofu.interop.CE3Kernel.unliftEffect")
