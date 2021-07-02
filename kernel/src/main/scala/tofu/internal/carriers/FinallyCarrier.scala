package tofu.internal.carriers

import tofu.Finally
import tofu.internal.WBInterop

abstract class FinallyCarrier[F[_], E] {
  type Exit[_]
  val content: Finally[F, Exit]
}

object FinallyCarrier {
  type Aux[F[_], E, Ex[_]] = FinallyCarrier[F, E] { type Exit[a] = Ex[a] }
  def apply[F[_], E, Ex[_]](fin: Finally[F, Ex]) = new FinallyCarrier[F, E] {
    type Exit[a] = Ex[a]
    val content = fin
  }

  final implicit def fromBracket[F[_], E, Exit[_]]: Aux[F, E, Exit] =
    macro WBInterop.delegate1[F, E, { val `tofu.interop.CE2Kernel.finallyFromBracket`: Unit }]
}
