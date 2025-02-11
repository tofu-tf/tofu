package tofu.internal.instances

import tofu.higherKind
import tofu.higherKind.RepresentableK
import tofu.time.TimeZone

private[tofu] trait TimeZoneInstance {
  implicit val timeZoneRepresentableK: RepresentableK[TimeZone] = higherKind.derived.genRepresentableK[TimeZone]
}
