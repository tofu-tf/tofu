package tofu.internal
package instances

import cats.~>
import tofu.higherKind.{RepK, RepresentableK}
import tofu.internal.carriers.{SleepCE2Carrier, SleepCE3Carrier}
import tofu.time.Sleep

import scala.compiletime.summonFrom
import scala.concurrent.duration.FiniteDuration

private[tofu] trait SleepInstance {
  inline given [F[_]]: Sleep[F] =
    summonFrom {
      case carrier: SleepCE3Carrier[F] => carrier
      case carrier: SleepCE2Carrier[F] => carrier
    }

  // TODO: use higherKind.derived macro when it is ready for scala 3
  given sleepRepresentableK: RepresentableK[Sleep] = new RepresentableK[Sleep] {
    def tabulate[F[_]](hom: RepK[Sleep, _] ~> F): Sleep[F] = new Sleep[F] {
      def sleep(duration: FiniteDuration): F[Unit] = hom(RepK[Sleep](_.sleep(duration)))
    }
  }
}
