package tofu.internal
package instances

import tofu.internal.carriers.{PerformCarrier2, PerformCarrier2Context, PerformCarrier3}
import tofu.kernel.types.PerformThrow

private[tofu] trait PerformInstance  extends PerformInstance1 {
  final implicit def interopCE3[F[_]](implicit carrier: PerformCarrier3[F]): PerformThrow[F] = carrier
}
private[tofu] trait PerformInstance1 extends PerformInstance2 {
  final implicit def interopCE2[F[_]](implicit carrier: PerformCarrier2[F]): PerformThrow[F] = carrier
}

private[tofu] trait PerformInstance2 {
  final implicit def interopCE2Contextual[F[_]](implicit
      carrier: PerformCarrier2Context[F]
  ): PerformThrow[F] = carrier
}
