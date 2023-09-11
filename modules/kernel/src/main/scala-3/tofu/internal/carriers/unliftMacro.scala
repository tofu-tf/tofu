package tofu.internal.carriers

import tofu.internal.Interop

trait UnliftCarrier2Macro:
  implicit inline def unliftIOEffect[F[_], G[_]]: UnliftCarrier2[F, G] =
    Interop.delegate2[F, G, UnliftCarrier2[F, G]]("tofu.interop.CE2Kernel.unliftEffect")

trait UnliftCarrier3Macro:
  implicit inline def unliftIOEffect[F[_], G[_]]: UnliftCarrier3[F, G] =
    Interop.delegate2[F, G, UnliftCarrier3[F, G]]("tofu.interop.CE3Kernel.unliftEffect")
