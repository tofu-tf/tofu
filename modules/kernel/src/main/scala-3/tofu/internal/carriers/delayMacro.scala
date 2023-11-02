package tofu.internal
package carriers

import tofu.internal.Interop

trait DelayCarrier2Macro:
  inline given interop[F[_]]: DelayCarrier2[F] =
    Interop.delegate1[F, DelayCarrier2[F]]("tofu.interop.CE2Kernel.delayViaSync")

trait DelayCarrier3Macro:
  inline given interop[F[_]]: DelayCarrier3[F] =
    Interop.delegate1[F, DelayCarrier3[F]]("tofu.interop.CE3Kernel.delayViaSync")
