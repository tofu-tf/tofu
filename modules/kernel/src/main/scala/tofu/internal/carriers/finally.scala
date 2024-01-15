package tofu.internal.carriers

import tofu.Finally

trait FinallyCarrier2[F[_], E] {
  type Exit[_]
  def content: Finally[F, Exit]

  def widen[Ex[x] >: Exit[x]]: FinallyCarrier2.Aux[F, E, Ex] =
    this.asInstanceOf[FinallyCarrier2.Aux[F, E, Ex]]
}

object FinallyCarrier2 extends FinallyCarrier2Macro {
  type Aux[F[_], E, Ex[_]] = FinallyCarrier2[F, E] { type Exit[a] = Ex[a] }

  trait Impl[F[_], E, Ex[_]] extends FinallyCarrier2[F, E] with Finally[F, Ex] {
    type Exit[a] = Ex[a]
    def content: Finally[F, Ex] = this
  }

}

trait FinallyCarrier3[F[_], E] {
  type Exit[_]
  def content: Finally[F, Exit]

  def widen[Ex[x] >: Exit[x]]: FinallyCarrier3.Aux[F, E, Ex] =
    this.asInstanceOf[FinallyCarrier3.Aux[F, E, Ex]]
}

object FinallyCarrier3 extends FinallyCarrier3Macro {
  type Aux[F[_], E, Ex[_]] = FinallyCarrier3[F, E] { type Exit[a] = Ex[a] }

  trait Impl[F[_], E, Ex[_]] extends FinallyCarrier3[F, E] with Finally[F, Ex] {
    type Exit[a] = Ex[a]
    def content: Finally[F, Exit] = this
  }

}
