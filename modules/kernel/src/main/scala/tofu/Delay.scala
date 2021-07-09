package tofu

import tofu.internal.EffectComp
import tofu.internal.Interop

trait Delay[F[_]] {
  def delay[A](a: => A): F[A]
}

object Delay extends CatsDelay with EffectComp[Delay] {
  type Safe[F[_, _]]  = Delay[F[Nothing, *]]
  type Catch[F[_, _]] = Delay[F[Throwable, *]]
}

class CatsDelay {
  implicit def byCatsSync[F[_]]: Delay[F] =
    macro Interop.delegate[Delay[F], F, { val `tofu.interop.CE2Kernel.delayViaSync`: Unit }]
}
