package tofu.internal
package instances

import tofu.ScopedExecute
import tofu.internal.carriers.{ScopedCarrier2, ScopedCarrier3}

private[tofu] trait ScopedInstance extends ScopedInstance0 {

  final implicit def interopCE3[Tag, F[_]](implicit carrier: ScopedCarrier3[Tag, F]): ScopedExecute[Tag, F] = carrier
}

private[tofu] trait ScopedInstance0 {
  final implicit def interopCE2[Tag, F[_]](implicit carrier: ScopedCarrier2[Tag, F]): ScopedExecute[Tag, F] = carrier
}
