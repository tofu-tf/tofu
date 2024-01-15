package tofu.internal
package instances

import tofu.BoundedParallel
import tofu.internal.carriers.{BoundedParallelCarrierCE2, BoundedParallelCarrierCE3}

import scala.compiletime.summonFrom

private[tofu] trait BoundedParallelInstance:
  inline given [F[_]]: BoundedParallel[F] = summonFrom {
    case carrier: BoundedParallelCarrierCE3[F] => carrier.content
    case carrier: BoundedParallelCarrierCE2[F] => carrier.content
  }
