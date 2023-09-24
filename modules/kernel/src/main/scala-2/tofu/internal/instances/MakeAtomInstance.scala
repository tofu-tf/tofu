package tofu.internal
package instances

import tofu.concurrent.MakeAtom
import tofu.internal.carriers.{MkAtomCE2Carrier, MkAtomCE3Carrier}

private[tofu] trait MakeAtomInstance extends MakeAtomInstance0 {
  final implicit def interopCE3[I[_], F[_]](implicit carrier: MkAtomCE3Carrier[I, F]): MakeAtom[I, F] = carrier
}

private[tofu] trait MakeAtomInstance0 {
  final implicit def interopCE2[I[_], F[_]](implicit carrier: MkAtomCE2Carrier[I, F]): MakeAtom[I, F] = carrier
}
