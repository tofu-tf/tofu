package tofu.internal.carriers

import tofu.internal.Interop

trait PerformCarrier2Macro {
  final implicit def interop2Effect[F[_]]: PerformCarrier2[F] =
    macro Interop.delegate[PerformCarrier2[F], F, { val `tofu.interop.CE2Kernel.performEffect`: Unit }]
}

trait PerformCarrier2ContextMacro {
  final implicit def interop2ContextEffect[F[_]]: PerformCarrier2Context[F] =
    macro Interop.delegate[
      PerformCarrier2Context[F],
      F, {
        val `tofu.interop.CE2Kernel.performContextConcurrentEffect`: Unit
      }
    ]
}

trait PerformCarrier3Macro {

  final implicit def interop3IO[F[_]]: PerformCarrier3[F] =
    macro Interop.delegate[PerformCarrier3[F], F, { val `tofu.interop.CE3Kernel.performDispatchContext`: Unit }]
}