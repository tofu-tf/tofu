package tofu.internal
package instances

import cats.MonadError
import tofu.Finally
import tofu.Guarantee
import tofu.internal.carriers.{FinallyCarrier2, FinallyCarrier3}

import scala.compiletime.summonFrom


private[tofu] trait GuaranteeInstance:
  inline given [F[_], E, Exit[_]](using inline m: MonadError[F, E]): Finally[F, Exit] = summonFrom {
    case carrier: FinallyCarrier3.Aux[F, E, Exit] => carrier.content
    case carrier: FinallyCarrier2.Aux[F, E, Exit] => carrier.content
  }
