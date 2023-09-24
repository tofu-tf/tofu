package tofu.internal.instances

import tofu.internal.carriers.TimeoutCE2Carrier
import tofu.internal.carriers.TimeoutCE3Carrier
import tofu.time.Timeout

trait TimeoutInstance extends TimeoutInstance0 {
  implicit def ce3Interop[F[_]](implicit timeout: TimeoutCE3Carrier[F]): Timeout[F] = timeout
}

trait TimeoutInstance0 {
  implicit def ce2Interop[F[_]](implicit timeout: TimeoutCE2Carrier[F]): Timeout[F] = timeout
}
