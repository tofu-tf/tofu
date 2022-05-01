package tofu.internal.carriers

import tofu.Wander
import tofu.internal.WBInterop

abstract class WanderCarrierCE2[F[_]] {
  def content: Wander[F]
}
object WanderCarrierCE2               {
  trait Impl[F[_]] extends WanderCarrierCE2[F] with Wander[F] {
    final def content: Wander[F] = this
  }

  implicit def wanderForCE2[F[_]]: WanderCarrierCE2[F] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE2Kernel.wander`: Unit }]
}

abstract class WanderCarrierCE3[F[_]] {
  def content: Wander[F]
}
object WanderCarrierCE3               {
  trait Impl[F[_]] extends WanderCarrierCE3[F] with Wander[F] {
    final def content: Wander[F] = this
  }

  implicit def wanderForCE3[F[_]]: WanderCarrierCE3[F] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE3Kernel.wander`: Unit }]
}
