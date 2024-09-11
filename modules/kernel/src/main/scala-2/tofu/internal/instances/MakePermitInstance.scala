package tofu.internal.instances

import tofu.concurrent.MakePermit
import tofu.internal.carriers.{MkPermitCE2Carrier, MkPermitCE3Carrier}

private[tofu] trait MakePermitInstance extends MakePermitInstance0 {
  final implicit def interopCE3[I[_], F[_]](implicit carrier: MkPermitCE3Carrier[I, F]): MakePermit[I, F] =
    carrier
}

private[tofu] trait MakePermitInstance0 {
  final implicit def interopCE2[I[_], F[_]](implicit carrier: MkPermitCE2Carrier[I, F]): MakePermit[I, F] =
    carrier
}
