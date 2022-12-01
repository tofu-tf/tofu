package tofu.time

import tofu.internal.carriers.TimeoutCE3Carrier
import tofu.internal.carriers.TimeoutCE2Carrier
import tofu.internal.EffectComp
import scala.concurrent.duration.FiniteDuration

trait Timeout[F[_]] {
  def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A]
}

object Timeout extends TimeoutInterop with EffectComp[Timeout]

trait TimeoutInterop extends TimeoutInterop1 {
  implicit def ce3Interop[F[_]](implicit timeout: TimeoutCE3Carrier[F]): Timeout[F] = timeout
}

trait TimeoutInterop1 {
  implicit def ce2Interop[F[_]](implicit timeout: TimeoutCE2Carrier[F]): Timeout[F] = timeout
}
