package tofu.generate

import tofu.higherKind
import tofu.higherKind.RepresentableK

trait GetRandomInstances {

  implicit val genRandomRepresentableK: RepresentableK[GenRandom] = higherKind.derived.genRepresentableK[GenRandom]
}
