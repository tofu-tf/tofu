package tofu.internal
package carriers

trait FinallyCarrier2Macro {
  final implicit def fromBracket[F[_], E, Exit[_]]: FinallyCarrier2.Aux[F, E, Exit] =
    macro WBInterop.delegate1[F, E, { val `tofu.interop.CE2Kernel.finallyFromBracket`: Unit }]
}

trait FinallyCarrier3Macro {
  final implicit def fromBracket[F[_], E, Exit[_]]: FinallyCarrier3.Aux[F, E, Exit] =
    macro WBInterop.delegate1[F, E, { val `tofu.interop.CE3Kernel.finallyFromBracket`: Unit }]
}