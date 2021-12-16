package tofu.internal.carriers

import tofu.kernel.types.Perform
import tofu.internal.Interop

trait PerformCarrier2[F[_]] extends Perform[F, Throwable]

object PerformCarrier2 {
  final implicit def interop2Effect[F[_]]: PerformCarrier2[F] =
    macro Interop.delegate[PerformCarrier2[F], F, { val `tofu.interop.CE2Kernel.performEffect`: Unit }]
}

trait PerformCarrier2Context[F[_]] extends Perform[F, Throwable]

object PerformCarrier2Context {
  final implicit def interop2ContextEffect[F[_]]: PerformCarrier2Context[F] =
    macro Interop.delegate[
      PerformCarrier2Context[F],
      F, {
        val `tofu.interop.CE2Kernel.performContextEffect`: Unit
      }
    ]
}

trait PerformCarrier3[F[_]] extends Perform[F, Throwable]

object PerformCarrier3 {
  final implicit def interop3IO[F[_]]: PerformCarrier3[F] =
    macro Interop.delegate[PerformCarrier3[F], F, { val `tofu.interop.CE3Kernel.performDispatch`: Unit }]
}
