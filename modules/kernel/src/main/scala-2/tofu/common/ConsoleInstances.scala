package tofu.common

import tofu.higherKind.{RepresentableK, derived}

trait ConsoleInstances {
  implicit val representableKInstance: RepresentableK[Console] = derived.genRepresentableK[Console]
}
