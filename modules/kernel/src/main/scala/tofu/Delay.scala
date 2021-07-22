package tofu

import tofu.internal.EffectComp
import tofu.internal.carriers.{DelayCarrier2, DelayCarrier3}

trait Delay[F[_]] {
  def delay[A](a: => A): F[A]
}

object Delay extends CatsDelay with EffectComp[Delay] {
  type Safe[F[_, _]]  = Delay[F[Nothing, *]]
  type Catch[F[_, _]] = Delay[F[Throwable, *]]
}

class CatsDelay {
  final implicit def interopCE2[F[_]](implicit carrier: DelayCarrier2[F]): Delay[F] = carrier
  final implicit def interopCE3[F[_]](implicit carrier: DelayCarrier3[F]): Delay[F] = carrier
}
