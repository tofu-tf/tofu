package tofu.internal
package instances

import tofu.concurrent.MakeAgent
import tofu.internal.carriers.{MkAgentCE2Carrier, MkAgentCE3Carrier}

private[tofu] trait MakeAgentInstance extends MakeAgentInstance0 {
  final implicit def interopCE3[I[_], F[_]](implicit carrier: MkAgentCE3Carrier[I, F]): MakeAgent[I, F] = carrier
}

private[tofu] trait MakeAgentInstance0 {
  final implicit def interopCE2[I[_], F[_]](implicit carrier: MkAgentCE2Carrier[I, F]): MakeAgent[I, F] = carrier  
}
