package tofu.generate

trait GetRandomInstances {

  implicit val genRandomRepresentableK: RepresentableK[GenRandom] = higherKind.derived.genRepresentableK[GenRandom]
}
