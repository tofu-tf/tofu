package tofu.internal
package instances

import scala.compiletime.summonFrom
import tofu.concurrent.MakePermit
import tofu.internal.carriers.{MkPermitCE2Carrier, MkPermitCE3Carrier}

private[tofu] trait MakePermitInstance:
  inline given [I[_], F[_]]: MakePermit[I, F] = summonFrom {
    case carrier: MkPermitCE2Carrier[I, F] => carrier
    case carrier: MkPermitCE3Carrier[I, F] => carrier
  }
