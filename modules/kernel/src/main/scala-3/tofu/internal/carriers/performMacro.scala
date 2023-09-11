package tofu.internal.carriers

import tofu.internal.Interop

trait PerformCarrier2Macro:
  implicit inline def interop2Effect[F[_]]: PerformCarrier2[F] =
    Interop.delegate1[F, PerformCarrier2[F]]("tofu.interop.CE2Kernel.performEffect")

trait PerformCarrier2ContextMacro:
  implicit inline def interop2ContextEffect[F[_]]: PerformCarrier2Context[F] =
    Interop.delegate1[F, PerformCarrier2Context[F]]("tofu.interop.CE2Kernel.performContextConcurrentEffect")

trait PerformCarrier3Macro:
  implicit inline def interop3IO[F[_]]: PerformCarrier3[F] =
    Interop.delegate1[F, PerformCarrier3[F]]("tofu.interop.CE2Kernel.performDispatchContext")
