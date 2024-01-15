package tofu.internal
package instances

import tofu.internal.carriers.ClockCE3Carrier
import tofu.internal.carriers.ClockCE2Carrier
import tofu.time.Clock

import scala.compiletime.summonFrom

private[tofu] trait ClockInstance:
  inline given [F[_]]: Clock[F] = summonFrom {
    case carrier: ClockCE3Carrier[F] => carrier
    case carrier: ClockCE2Carrier[F] => carrier
  }
