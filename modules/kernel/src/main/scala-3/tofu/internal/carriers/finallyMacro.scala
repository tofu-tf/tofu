package tofu.internal
package carriers

object FinallyCarrier2Macro:
  inline val method = "tofu.interop.CE2Kernel.finallyFromBracket"

trait FinallyCarrier2Macro extends FinallyCarrier2MacroLowLevel:
  inline given fromBracketAny[F[_], E]: FinallyCarrier2.Aux[F, E, [x] =>> Any] =
    Interop
      .delegate1_0[F, E, FinallyCarrier2[F, E]](FinallyCarrier2Macro.method)
      .widen

trait FinallyCarrier2MacroLowLevel:
  inline given fromBracket[F[_], E, Exit[_]]: FinallyCarrier2.Aux[F, E, Exit] =
    Interop.delegate1_0[F, E, FinallyCarrier2.Aux[F, E, Exit]](FinallyCarrier2Macro.method)

object FinallyCarrier3Macro:
  inline val method = "tofu.interop.CE3Kernel.finallyFromBracket"

trait FinallyCarrier3Macro extends FinallyCarrier3MacroLowLevel:
  inline given fromBracketAny[F[_], E]: FinallyCarrier3.Aux[F, E, [x] =>> Any] =
    Interop
      .delegate1_0[F, E, FinallyCarrier3[F, E]](FinallyCarrier3Macro.method)
      .widen

trait FinallyCarrier3MacroLowLevel:
  inline given fromBracket[F[_], E, Exit[_]]: FinallyCarrier3.Aux[F, E, Exit] =
    Interop.delegate1_0[F, E, FinallyCarrier3.Aux[F, E, Exit]](FinallyCarrier3Macro.method)
