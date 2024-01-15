package tofu.internal.carriers

import tofu.internal.Interop

trait BoundedParallelCarrierCE2Macro:
  inline given boundedParallelForCE2[F[_]]: BoundedParallelCarrierCE2[F] =
    Interop.delegate1[F, BoundedParallelCarrierCE2[F]]("tofu.interop.CE2Kernel.boundedParallel")

trait BoundedParallelCarrierCE3Macro:
  inline given boundedParallelForCE3[F[_]]: BoundedParallelCarrierCE3[F] =
    Interop.delegate1[F, BoundedParallelCarrierCE3[F]]("tofu.interop.CE3Kernel.boundedParallel")
