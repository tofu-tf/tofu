package tofu.internal
package instances

import tofu.concurrent.MakeQVar
import tofu.internal.carriers.{MkQVarCE2Carrier, MkQVarCE3Carrier}

private[tofu] trait MakeQVarInstance extends MakeQVarInstance0 {
    final implicit def interopCE3[I[_], F[_]](implicit carrier: MkQVarCE3Carrier[I, F]): MakeQVar[I, F] = carrier
}

private[tofu] trait MakeQVarInstance0 {
  final implicit def interopCE2[I[_], F[_]](implicit carrier: MkQVarCE2Carrier[I, F]): MakeQVar[I, F] = carrier
}
