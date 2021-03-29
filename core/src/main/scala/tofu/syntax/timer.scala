package tofu.syntax


import scala.concurrent.duration.FiniteDuration
import cats.effect.Temporal

object timer {
  def sleep[F[_]](duration: FiniteDuration)(implicit timer: Temporal[F]): F[Unit] = timer.sleep(duration)
}
