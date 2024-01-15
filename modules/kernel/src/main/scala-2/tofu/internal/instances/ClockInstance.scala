package tofu.internal
package instances

import tofu.internal.carriers.ClockCE3Carrier
import tofu.internal.carriers.ClockCE2Carrier
import tofu.time.Clock

private[tofu] trait ClockInstance extends ClockInstance0 {
  implicit def ce3Interop[F[_]](implicit clock: ClockCE3Carrier[F]): Clock[F] = clock
}

private[tofu] trait ClockInstance0 {
  implicit def ce2Interop[F[_]](implicit clock: ClockCE2Carrier[F]): Clock[F] = clock
}
