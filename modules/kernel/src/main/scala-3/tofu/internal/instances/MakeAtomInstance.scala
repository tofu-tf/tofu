package tofu.internal
package instances

import scala.compiletime.summonFrom

import tofu.concurrent.MakeAtom
import tofu.internal.carriers.{MkAtomCE2Carrier, MkAtomCE3Carrier}

private[tofu] trait MakeAtomInstance:
  inline given [I[_], F[_]]: MakeAtom[I, F] = summonFrom {
    case carrier: MkAtomCE3Carrier[I, F] => carrier
    case carrier: MkAtomCE2Carrier[I, F] => carrier
  }
