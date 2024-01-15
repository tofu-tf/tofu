package tofu.internal
package instances

import tofu.Delay
import tofu.internal.carriers.{DelayCarrier2, DelayCarrier3}

import scala.compiletime.summonFrom

private[tofu] trait DelayInstance:
  inline given [F[_]]: Delay[F] = summonFrom {
    case carrier: DelayCarrier3[F] => carrier
    case carrier: DelayCarrier2[F] => carrier
  }
