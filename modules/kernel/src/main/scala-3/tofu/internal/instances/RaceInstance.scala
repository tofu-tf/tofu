package tofu.internal
package instances

import cats.MonadError
import tofu.Race
import tofu.internal.carriers.{FibersCarrier2, FibersCarrier3}

import scala.compiletime.summonFrom

private[tofu] trait RaceInstance:
  inline given [F[_], E](using inline m: MonadError[F, E]): Race[F] = summonFrom {
    case carrier: FibersCarrier3[F, E] => carrier.content
    case carrier: FibersCarrier2[F]    => carrier.content
  }
