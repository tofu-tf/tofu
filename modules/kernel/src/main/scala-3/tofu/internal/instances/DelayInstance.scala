package tofu.internal
package instances

import cats.~>
import tofu.Delay
import tofu.higherKind.{RepK, RepresentableK}
import tofu.internal.carriers.{DelayCarrier2, DelayCarrier3}

import scala.compiletime.summonFrom

private[tofu] trait DelayInstance:
  inline given [F[_]]: Delay[F] = summonFrom {
    case carrier: DelayCarrier3[F] => carrier
    case carrier: DelayCarrier2[F] => carrier
  }

  // TODO: use higherKind.derived macro when it is ready for scala 3
  given delayRepresentableK: RepresentableK[Delay] = new RepresentableK[Delay] {
    def tabulate[F[_]](hom: RepK[Delay, _] ~> F): Delay[F] = new Delay[F] {
      def delay[A](a: => A): F[A] = hom(RepK[Delay](_.delay(a)))
    }
  }
