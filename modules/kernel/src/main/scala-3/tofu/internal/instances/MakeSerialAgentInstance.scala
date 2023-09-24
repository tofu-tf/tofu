package tofu.internal
package instances

import scala.compiletime.summonFrom

import tofu.concurrent.MakeSerialAgent
import tofu.internal.carriers.{MkSerialAgentCE2Carrier, MkSerialAgentCE3Carrier}

private[tofu] trait MakeSerialAgentInstance:
  inline given [I[_], F[_]]: MakeSerialAgent[I, F] = summonFrom {
    case carrier: MkSerialAgentCE3Carrier[I, F] => carrier
    case carrier: MkSerialAgentCE2Carrier[I, F] => carrier
  }
