package tofu.internal.carriers

import tofu.Finally
import tofu.internal.WBInterop

trait FinallyCarrier2[F[_], E] {
  type Exit[_]
  def content: Finally[F, Exit]
}

object FinallyCarrier2 {
  type Aux[F[_], E, Ex[_]] = FinallyCarrier2[F, E] { type Exit[a] = Ex[a] }

  trait Impl[F[_], E, Ex[_]] extends FinallyCarrier2[F, E] with Finally[F, Ex] {
    type Exit[a] = Ex[a]
    def content = this
  }

  final implicit def fromBracket[F[_], E, Exit[_]]: Aux[F, E, Exit] =
    macro WBInterop.delegate1[F, E, { val `tofu.interop.CE2Kernel.finallyFromBracket`: Unit }]
}

trait FinallyCarrier3[F[_], E] {
  type Exit[_]
  def content: Finally[F, Exit]
}

object FinallyCarrier3 {
  type Aux[F[_], E, Ex[_]] = FinallyCarrier3[F, E] { type Exit[a] = Ex[a] }

  trait Impl[F[_], E, Ex[_]] extends FinallyCarrier3[F, E] with Finally[F, Ex] {
    type Exit[a] = Ex[a]
    def content = this
  }

  final implicit def fromBracket[F[_], E, Exit[_]]: Aux[F, E, Exit] =
    macro WBInterop.delegate1[F, E, { val `tofu.interop.CE3Kernel.finallyFromBracket`: Unit }]
}
