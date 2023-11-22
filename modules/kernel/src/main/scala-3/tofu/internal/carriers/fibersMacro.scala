package tofu.internal
package carriers

object FibersCarrier2Macro:
  val method = "tofu.interop.CE2Kernel.startFromConcurrent"

trait FibersCarrier2Macro:
  inline given startFromConcurrentAny[F[_]]: FibersCarrier2.Aux[F, [x] =>> Any, [x] =>> Any] =
    Interop
      .delegate1[F, FibersCarrier2[F]](FibersCarrier2Macro.method)
      .asInstanceOf[FibersCarrier2.Aux[F, [x] =>> Any, [x] =>> Any]]

  inline given startFromConcurrentSome[F[_], Fiber[_]]: FibersCarrier2.Aux[F, [x] =>> x, Fiber[_]] =
    Interop.delegate1[F, FibersCarrier2.Aux[F, [x] =>> x, Fiber]](FibersCarrier2Macro.method)

trait FibersCarrier2MacroLowLevel:
  inline given startFromConcurrent[F[_], Exit[_], Fiber[_]]: FibersCarrier2.Aux[F, Exit, Fiber] =
    Interop.delegate1[F, FibersCarrier2.Aux[F, Exit, Fiber]](FibersCarrier2Macro.method)

object FibersCarrier3Macro:
  val method = "tofu.interop.CE3Kernel.startFromConcurrent"

trait FibersCarrier3Macro extends FibersCarrier3MacroLowLevel:
  inline given startFromConcurrentAny[F[_], E]: FibersCarrier3.Aux[F, E, [x] =>> Any, [x] =>> Any] =
    Interop
      .delegate1_0[F, E, FibersCarrier3[F, E]](FibersCarrier3Macro.method)
      .asInstanceOf[FibersCarrier3.Aux[F, E, [x] =>> Any, [x] =>> Any]]

trait FibersCarrier3MacroLowLevel:
  inline given startFromConcurrent[F[_], E, Exit[_], Fiber[_]]: FibersCarrier3.Aux[F, E, Exit, Fiber] =
    Interop.delegate1_0[F, E, FibersCarrier3.Aux[F, E, Exit, Fiber]](FibersCarrier3Macro.method)
