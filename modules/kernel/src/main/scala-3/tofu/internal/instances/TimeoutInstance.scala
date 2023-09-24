package tofu.internal
package instances

import tofu.internal.carriers.{TimeoutCE2Carrier, TimeoutCE3Carrier}
import tofu.time.Timeout

import scala.compiletime.summonFrom

private[tofu] trait TimeoutInstance:
  inline given [F[_]]: Timeout[F] = summonFrom {
    case carrier: TimeoutCE3Carrier[F] => carrier
    case carrier: TimeoutCE2Carrier[F] => carrier
  }
