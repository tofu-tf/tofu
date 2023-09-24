package tofu.internal
package instances

import tofu.concurrent.MakeSerialAgent
import tofu.internal.carriers.{MkSerialAgentCE2Carrier, MkSerialAgentCE3Carrier}

private[tofu] trait MakeSerialAgentInstance extends MakeSerialAgentInstance0 {
    final implicit def interopCE3[I[_], F[_]](implicit carrier: MkSerialAgentCE3Carrier[I, F]): MakeSerialAgent[I, F] =
    carrier
}

private[tofu] trait MakeSerialAgentInstance0 {
  final implicit def interopCE2[I[_], F[_]](implicit carrier: MkSerialAgentCE2Carrier[I, F]): MakeSerialAgent[I, F] =
    carrier

}
