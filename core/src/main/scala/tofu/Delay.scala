package tofu

import cats.{Applicative, Defer}

trait Delay[F[_]] {
  def delay[A](a: => A): F[A]
}

object Delay extends CatsDelay {
  type Safe[F[_, _]]  = Delay[F[Nothing, *]]
  type Catch[F[_, _]] = Delay[F[Throwable, *]]
}

class CatsDelay {
  implicit def byCatsDefer[F[_]](implicit FD: Defer[F], F: Applicative[F]): Delay[F] =
    new Delay[F] {
      def delay[A](a: => A): F[A] = FD.defer(F.pure(a))
    }
}
