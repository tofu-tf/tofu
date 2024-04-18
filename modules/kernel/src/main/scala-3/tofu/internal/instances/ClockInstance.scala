package tofu.internal
package instances

import cats.~>
import tofu.higherKind.{RepK, RepresentableK}
import tofu.internal.carriers.ClockCE3Carrier
import tofu.internal.carriers.ClockCE2Carrier
import tofu.time.Clock

import java.util.concurrent.TimeUnit
import scala.compiletime.summonFrom

private[tofu] trait ClockInstance {
  inline given [F[_]]: Clock[F] = summonFrom {
    case carrier: ClockCE3Carrier[F] => carrier
    case carrier: ClockCE2Carrier[F] => carrier
  }

  // TODO: use higherKind.derived macro when it is ready for scala 3
  given clockRepresentableK: RepresentableK[Clock] = new RepresentableK[Clock] {
    def tabulate[F[_]](hom: RepK[Clock, _] ~> F): Clock[F] = new Clock[F] {
      def realTime(unit: TimeUnit): F[Long] = hom(RepK[Clock](_.realTime(unit)))
      def nanos: F[Long]                    = hom(RepK[Clock](_.nanos))
    }
  }
}
