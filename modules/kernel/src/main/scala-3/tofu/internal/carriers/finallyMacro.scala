package tofu.internal
package carriers

trait FinallyCarrier2Macro:
  inline given fromBracket[F[_], E, Exit[_]]: FinallyCarrier2.Aux[F, E, Exit] =
    Interop.delegate1_0[F, E, FinallyCarrier2.Aux[F, E, Exit]]("tofu.interop.CE2Kernel.finallyFromBracket")

trait FinallyCarrier3Macro:
  inline given fromBracket[F[_], E, Exit[_]]: FinallyCarrier3.Aux[F, E, Exit] =
    Interop.delegate1_0[F, E, FinallyCarrier3.Aux[F, E, Exit]]("tofu.interop.CE3Kernel.finallyFromBracket")
