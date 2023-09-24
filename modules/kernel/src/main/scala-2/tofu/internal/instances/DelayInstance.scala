package tofu.internal.instances

import tofu.Delay
import tofu.internal.carriers.DelayCarrier3
import tofu.internal.carriers.DelayCarrier2

trait DelayInstance extends DelayInstance0 {
  final implicit def interopCE3[F[_]](implicit carrier: DelayCarrier3[F]): Delay[F] = carrier
}

trait DelayInstance0 {
  final implicit def interopCE2[F[_]](implicit carrier: DelayCarrier2[F]): Delay[F] = carrier
}
