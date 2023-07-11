package tofu.generate

import tofu.higherKind
import tofu.higherKind.RepresentableK

trait GenUUIDInstances {
  implicit val genUUIDRepresentableK: RepresentableK[GenUUID] = higherKind.derived.genRepresentableK[GenUUID]
}
