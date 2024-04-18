package tofu.internal
package instances

import tofu.higherKind
import tofu.higherKind.RepresentableK
import tofu.internal.carriers.{ClockCE2Carrier, ClockCE3Carrier}
import tofu.time.Clock

private[tofu] trait ClockInstance extends ClockInstance0 {
  implicit def ce3Interop[F[_]](implicit clock: ClockCE3Carrier[F]): Clock[F] = clock
}

private[tofu] trait ClockInstance0 {
  implicit def ce2Interop[F[_]](implicit clock: ClockCE2Carrier[F]): Clock[F] = clock

  implicit val clockRepresentableK: RepresentableK[Clock] = higherKind.derived.genRepresentableK[Clock]
}
