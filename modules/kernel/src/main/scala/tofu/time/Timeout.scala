package tofu.time

import tofu.internal.carriers.TimeoutCE3Carrier
import tofu.internal.carriers.TimeoutCE2Carrier
import tofu.internal.EffectComp
import tofu.internal.instances.TimeoutInstance
import scala.concurrent.duration.FiniteDuration

trait Timeout[F[_]] {
  def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A]
}

object Timeout extends TimeoutInstance with EffectComp[Timeout]
