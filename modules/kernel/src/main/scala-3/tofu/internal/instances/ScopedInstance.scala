package tofu.internal
package instances

import scala.compiletime.summonFrom

import tofu.ScopedExecute
import tofu.internal.carriers.{ScopedCarrier2, ScopedCarrier3}

private[tofu] trait ScopedInstance :
  inline given [Tag, F[_]]: ScopedExecute[Tag, F] = summonFrom {
    case carrier: ScopedCarrier3[Tag, F] => carrier
    case carrier: ScopedCarrier2[Tag, F] => carrier
  }
