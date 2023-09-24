package tofu.internal
package instances

import scala.compiletime.summonFrom

import tofu.concurrent.MakeQVar
import tofu.internal.carriers.{MkQVarCE2Carrier, MkQVarCE3Carrier}

private[tofu] trait MakeQVarInstance:
  inline given [I[_], F[_]]: MakeQVar[I, F] = summonFrom {
    case carrier: MkQVarCE3Carrier[I, F] => carrier
    case carrier: MkQVarCE2Carrier[I, F] => carrier
  }
