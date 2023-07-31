package tofu.internal.carriers

import tofu.compat.unused212

trait BoundedParallelCarrierCE2Macro {

  implicit inline def boundedParallelForCE2[F[_]]: BoundedParallelCarrierCE2[F] =
    TestMacro.delegate0[F, BoundedParallelCarrierCE2]("tofu.interop.CE2Kernel.boundedParallel")
}

trait BoundedParallelCarrierCE3Macro {

  implicit inline def boundedParallelForCE3[F[_]]: BoundedParallelCarrierCE3[F] =
    TestMacro.delegate0[F, BoundedParallelCarrierCE3]("tofu.interop.CE3Kernel.boundedParallel")

}