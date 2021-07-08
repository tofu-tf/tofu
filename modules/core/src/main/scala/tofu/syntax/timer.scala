package tofu.syntax

import cats.effect.Timer

import scala.concurrent.duration.FiniteDuration

object timer {
  def sleep[F[_]](duration: FiniteDuration)(implicit timer: Timer[F]): F[Unit] = timer.sleep(duration)
}
