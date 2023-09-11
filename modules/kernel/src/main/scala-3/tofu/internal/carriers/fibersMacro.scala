package tofu.internal
package carriers

trait FibersCarrier2Macro:
  implicit inline def startFromConcurrent[F[_], Exit[_], Fiber[_]]: FibersCarrier2.Aux[F, Exit, Fiber] =
    Interop.delegate1[F, FibersCarrier2.Aux[F, Exit, Fiber]]("tofu.interop.CE2Kernel.startFromConcurrent")

trait FibersCarrier3Macro:
  implicit inline def startFromConcurrent[F[_], E, Exit[_], Fiber[_]]: FibersCarrier3.Aux[F, E, Exit, Fiber] =
    Interop.delegate1_0[F, E, FibersCarrier3.Aux[F, E, Exit, Fiber]]("tofu.interop.CE3Kernel.startFromConcurrent")
