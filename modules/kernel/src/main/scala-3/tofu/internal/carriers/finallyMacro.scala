package tofu.internal
package carriers

trait FinallyCarrier2Macro:
  implicit inline def fromBracket[F[_], E, Exit[_]]: FinallyCarrier2.Aux[F, E, Exit] =
    Interop.delegate1[F, FinallyCarrier2.Aux[F, E, Exit]]("tofu.interop.CE2Kernel.finallyFromBracket")

trait FinallyCarrier3Macro:
  implicit inline def fromBracket[F[_], E, Exit[_]]: FinallyCarrier3.Aux[F, E, Exit] =
    Interop.delegate1[F, FinallyCarrier3.Aux[F, E, Exit]]("tofu.interop.CE3Kernel.finallyFromBracket")
