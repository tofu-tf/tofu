package tofu.logging

import tofu.higherKind.RepresentableK
import tofu.higherKind

trait LoggingRepresentableKInstances {
  implicit val loggingRepresentable: RepresentableK[Logging] = higherKind.derived.genRepresentableK[Logging]
}
