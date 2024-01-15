package tofu.internal
package instances

import cats.MonadError
import tofu.Fibers
import tofu.internal.carriers.{FibersCarrier2, FibersCarrier3}

import scala.compiletime.summonFrom

private[tofu] trait FireInstance:
  inline given [F[_], E, Ex[_], Fib[_]](using inline m: MonadError[F, E]): Fibers[F, Ex, Fib] = summonFrom {
    case carrier: FibersCarrier3.Aux[F, E, Ex, Fib] => carrier.content
    case carrier: FibersCarrier2.Aux[F, Ex, Fib]    => carrier.content
  }
