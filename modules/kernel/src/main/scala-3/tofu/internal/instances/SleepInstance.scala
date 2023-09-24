package tofu.internal
package instances

import tofu.Delay
import tofu.internal.carriers.{SleepCE2Carrier, SleepCE3Carrier}
import tofu.time.Sleep

import scala.compiletime.summonFrom

private[tofu] trait SleepInstance:
  inline given [F[_]]: Sleep[F] =
    summonFrom {
      case carrier: SleepCE3Carrier[F] => carrier
      case carrier: SleepCE2Carrier[F] => carrier
    }
