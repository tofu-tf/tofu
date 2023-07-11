package tofu.internal.carriers

import tofu.Finally

trait FinallyCarrier2[F[_], E] {
  type Exit[_]
  def content: Finally[F, Exit]
}

object FinallyCarrier2 extends FinallyCarrier2Macro {
  type Aux[F[_], E, Ex[_]] = FinallyCarrier2[F, E] { type Exit[a] = Ex[a] }

  trait Impl[F[_], E, Ex[_]] extends FinallyCarrier2[F, E] with Finally[F, Ex] {
    type Exit[a] = Ex[a]
    def content = this
  }

}

trait FinallyCarrier3[F[_], E] {
  type Exit[_]
  def content: Finally[F, Exit]
}

object FinallyCarrier3 extends FinallyCarrier3Macro {
  type Aux[F[_], E, Ex[_]] = FinallyCarrier3[F, E] { type Exit[a] = Ex[a] }

  trait Impl[F[_], E, Ex[_]] extends FinallyCarrier3[F, E] with Finally[F, Ex] {
    type Exit[a] = Ex[a]
    def content = this
  }

}
