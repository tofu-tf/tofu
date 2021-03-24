package tofu

import cats.effect.Sync

trait Delay[F[_]] {
  def delay[A](a: => A): F[A]
}

object Delay extends CatsDelay {
  type Safe[F[_, _]]  = Delay[F[Nothing, *]]
  type Catch[F[_, _]] = Delay[F[Throwable, *]]
}

class CatsDelay {
  implicit def byCatsSync[F[_]](implicit FS: Sync[F]): Delay[F] =
    new Delay[F] {
      def delay[A](a: => A): F[A] = FS.delay(a)
    }
}
