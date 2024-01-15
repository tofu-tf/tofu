package tofu.internal
package instances

import scala.compiletime.summonFrom

import tofu.concurrent.MakeAgent
import tofu.internal.carriers.{MkAgentCE2Carrier, MkAgentCE3Carrier}

private[tofu] trait MakeAgentInstance:
  inline given [I[_], F[_]]: MakeAgent[I, F] = summonFrom {
    case carrier: MkAgentCE3Carrier[I, F] => carrier
    case carrier: MkAgentCE2Carrier[I, F] => carrier
  }
