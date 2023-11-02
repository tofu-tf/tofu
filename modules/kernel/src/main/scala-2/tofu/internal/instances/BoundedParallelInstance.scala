package tofu.internal.instances

import tofu.BoundedParallel
import tofu.internal.carriers.BoundedParallelCarrierCE3
import tofu.internal.carriers.BoundedParallelCarrierCE2

private[internal] trait BoundedParallelInstance extends BoundedParallelInstances0 {
  implicit def byCarrierCE3[F[_]](implicit carrier: BoundedParallelCarrierCE3[F]): BoundedParallel[F] = carrier.content
}

private[internal] trait BoundedParallelInstances0 {
  implicit def byCarrierCE2[F[_]](implicit carrier: BoundedParallelCarrierCE2[F]): BoundedParallel[F] = carrier.content
}
