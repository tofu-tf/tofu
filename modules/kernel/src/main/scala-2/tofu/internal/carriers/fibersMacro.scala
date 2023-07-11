package tofu.internal
package carriers

trait FibersCarrier2Macro {
  final implicit def startFromConcurrent[F[_], Exit[_], Fiber[_]]: FibersCarrier2.Aux[F, Exit, Fiber] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE2Kernel.startFromConcurrent`: Unit }]
}

trait FibersCarrier3Macro {
  final implicit def startFromConcurrent[F[_], E, Exit[_], Fiber[_]]: FibersCarrier3.Aux[F, E, Exit, Fiber] =
    macro WBInterop.delegate1[F, E, { val `tofu.interop.CE3Kernel.startFromConcurrent`: Unit }]
}


