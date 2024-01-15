package tofu.concurrent

import tofu.higherKind.{RepresentableK, derived}

trait QVarInstances {
  implicit def representableK[A]: RepresentableK[QVar[*[_], A]] = derived.genRepresentableK[QVar[*[_], A]]
}
