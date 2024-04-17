package tofu.time

import scala.concurrent.duration.FiniteDuration
import cats.data.Kleisli
import tofu.internal.EffectComp
import tofu.internal.instances.SleepInstance

trait Sleep[F[_]] {

  /** Pauses execution for desired duration
    */
  def sleep(duration: FiniteDuration): F[Unit]
}

object Sleep extends SleepInstance with EffectComp[Sleep] {
  implicit def sleepForKleisli[F[_], R](implicit s: Sleep[F]): Sleep[Kleisli[F, R, *]] =
    (duration: FiniteDuration) => Kleisli.liftF(s.sleep(duration))
}
