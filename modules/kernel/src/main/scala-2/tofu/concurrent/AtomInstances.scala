package tofu.concurrent

import tofu.higherKind.{RepresentableK, derived}

trait AtomInstances {
  implicit def representableKInstance[A]: RepresentableK[Atom[*[_], A]] = derived.genRepresentableK[Atom[*[_], A]]
}
