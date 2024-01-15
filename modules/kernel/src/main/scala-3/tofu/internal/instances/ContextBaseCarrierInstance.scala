package tofu.internal
package instances

import scala.compiletime.summonFrom

import tofu.internal.carriers.{UnliftCarrier2, UnliftCarrier3}
import tofu.lift.Unlift

private[tofu] trait ContextBaseCarrierInstance:
  inline given [F[_], G[_]]: Unlift[F, G] = summonFrom {
    case carrier: UnliftCarrier3[F, G] => carrier
    case carrier: UnliftCarrier2[F, G] => carrier
  }
