package tofu.internal
package instances

import scala.compiletime.summonFrom

import tofu.internal.carriers.{PerformCarrier2, PerformCarrier2Context, PerformCarrier3}
import tofu.kernel.types.PerformThrow

private[tofu] trait PerformInstance:
  inline given [F[_]]: PerformThrow[F] = summonFrom {
    case carrier: PerformCarrier3[F]        => carrier
    case carrier: PerformCarrier2[F]        => carrier
    case carrier: PerformCarrier2Context[F] => carrier
  }
