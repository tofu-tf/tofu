package tofu

import scala.concurrent.duration.FiniteDuration
import tofu.internal.EffectComp
import tofu.internal.Interop

trait Timeout[F[_]] {
  def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A]
}

object Timeout extends TimeoutImplicits with EffectComp[Timeout]

trait TimeoutImplicits { self: Timeout.type =>
  implicit def concurrent[F[_]]: Timeout[F] =
    macro Interop.delegate[Timeout[F], F, { val `tofu.interop.CE2Kernel.concurrentTimeout`: Unit }]
}
