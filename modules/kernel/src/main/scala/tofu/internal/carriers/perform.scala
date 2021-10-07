package tofu.internal.carriers

import tofu.kernel.types.Perform
import tofu.internal.Interop

trait PerformCarrier2[F[_], E] extends Perform[F, E]

object PerformCarrier2 {
  final implicit def interop2IO[F[_]]: PerformCarrier2[F, Throwable] =
    macro Interop.delegate[PerformCarrier2[F, Throwable], F, { val `tofu.interop.CE2Kernel.perform`: Unit }]
}

trait PerformCarrier3[F[_], E] extends Perform[F, E]

object PerformCarrier3 {
  // final implicit def interop3IO[F[_], E]: PerformCarrier2[F, E] =
  //   macro Interop.delegate1[PerformCarrier3[F, E], F, E, { val `tofu.interop.CE3Kernel.perform`: Unit }]
}
