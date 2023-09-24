package tofu

import tofu.internal.EffectComp
import tofu.internal.carriers.{DelayCarrier2, DelayCarrier3}
import tofu.internal.instances.DelayInstance

trait Delay[F[_]] {
  def delay[A](a: => A): F[A]
}

object Delay extends DelayInstance with EffectComp[Delay] {
  type Safe[F[_, _]]  = Delay[F[Nothing, _]]
  type Catch[F[_, _]] = Delay[F[Throwable, _]]
}
