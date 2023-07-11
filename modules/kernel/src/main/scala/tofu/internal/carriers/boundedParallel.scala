package tofu.internal.carriers

import tofu.BoundedParallel

abstract class BoundedParallelCarrierCE2[F[_]] {
  def content: BoundedParallel[F]
}

object BoundedParallelCarrierCE2 extends BoundedParallelCarrierCE2Macro {
  trait Impl[F[_]] extends BoundedParallelCarrierCE2[F] with BoundedParallel[F] {
    final def content: BoundedParallel[F] = this
  }
}

abstract class BoundedParallelCarrierCE3[F[_]] {
  def content: BoundedParallel[F]
}
object BoundedParallelCarrierCE3 extends BoundedParallelCarrierCE3Macro {
  trait Impl[F[_]] extends BoundedParallelCarrierCE3[F] with BoundedParallel[F] {
    final def content: BoundedParallel[F] = this
  }
}
