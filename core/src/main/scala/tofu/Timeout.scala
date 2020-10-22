package tofu

import cats.effect.{Concurrent, ContextShift, IO, Timer}
import simulacrum.typeclass
import tofu.syntax.feither._
import tofu.internal.NonTofu

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

@typeclass @nowarn("cat=unused-imports")
trait Timeout[F[_]] {
  def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A]
}

object Timeout extends LowPriorTimeoutImplicits {
  implicit def io(implicit timer: Timer[IO], cs: ContextShift[IO]): Timeout[IO] = new Timeout[IO] {
    override def timeoutTo[A](fa: IO[A], after: FiniteDuration, fallback: IO[A]): IO[A] = fa.timeoutTo(after, fallback)
  }
}

trait LowPriorTimeoutImplicits { self: Timeout.type =>
  implicit def concurrent[F[_]: NonTofu](implicit F: Concurrent[F], timer: Timer[F]): Timeout[F] =
    new Timeout[F] {
      override def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A] =
        F.race(timer.sleep(after), fa).getOrElseF(fallback)
    }
}
