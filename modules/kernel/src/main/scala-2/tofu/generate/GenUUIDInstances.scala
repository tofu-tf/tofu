package tofu.generate

trait GenUUIDInstances {
  implicit val genUUIDRepresentableK: RepresentableK[GenUUID] = higherKind.derived.genRepresentableK[GenUUID]
}
