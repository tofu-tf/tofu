package tofu.internal.carriers

import tofu.internal.WBInterop
import tofu.internal.carriers.{BoundedParallelCarrierCE2, BoundedParallelCarrierCE3}

trait BoundedParallelCarrierCE2Macro {

  implicit def boundedParallelForCE2[F[_]]: BoundedParallelCarrierCE2[F] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE2Kernel.boundedParallel`: Unit }]

}

trait BoundedParallelCarrierCE3Macro {

  implicit def boundedParallelForCE3[F[_]]: BoundedParallelCarrierCE3[F] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE3Kernel.boundedParallel`: Unit }]

}