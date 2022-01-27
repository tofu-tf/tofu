package tofu.syntax

import scala.concurrent.duration.FiniteDuration

import cats.effect.Timer

object timer {
  def sleep[F[_]](duration: FiniteDuration)(implicit timer: Timer[F]): F[Unit] = timer.sleep(duration)
}
