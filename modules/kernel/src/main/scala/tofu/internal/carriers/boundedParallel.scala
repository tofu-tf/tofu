package tofu.internal.carriers

import tofu.BoundedParallel
import tofu.internal.WBInterop

abstract class BoundedParallelCarrierCE2[F[_]] {
  def content: BoundedParallel[F]
}
object BoundedParallelCarrierCE2               {
  trait Impl[F[_]] extends BoundedParallelCarrierCE2[F] with BoundedParallel[F] {
    final def content: BoundedParallel[F] = this
  }

  implicit def boundedParallelForCE2[F[_]]: BoundedParallelCarrierCE2[F] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE2Kernel.boundedParallel`: Unit }]
}

abstract class BoundedParallelCarrierCE3[F[_]] {
  def content: BoundedParallel[F]
}
object BoundedParallelCarrierCE3               {
  trait Impl[F[_]] extends BoundedParallelCarrierCE3[F] with BoundedParallel[F] {
    final def content: BoundedParallel[F] = this
  }

  implicit def boundedParallelForCE3[F[_]]: BoundedParallelCarrierCE3[F] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE3Kernel.boundedParallel`: Unit }]
}
