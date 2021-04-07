package tofu

import cats.effect.Sync
import tofu.internal.EffectComp

trait Delay[F[_]] {
  def delay[A](a: => A): F[A]
}

object Delay extends CatsDelay with EffectComp[Delay] {
  type Safe[F[_, _]]  = Delay[F[Nothing, *]]
  type Catch[F[_, _]] = Delay[F[Throwable, *]]
}

class CatsDelay {
  implicit def byCatsSync[F[_]](implicit FS: Sync[F]): Delay[F] =
    new Delay[F] {
      def delay[A](a: => A): F[A] = FS.delay(a)
    }
}
