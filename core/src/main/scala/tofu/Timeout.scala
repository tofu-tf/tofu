package tofu

import cats.effect.{Concurrent, IO}
import simulacrum.typeclass
import tofu.compat.unused
import tofu.syntax.feither._
import tofu.internal.NonTofu

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration
import cats.effect.Temporal

@typeclass @nowarn("cat=unused-imports")
trait Timeout[F[_]] {
  def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A]
}

object Timeout extends LowPriorTimeoutImplicits {
  implicit def io(implicit timer: Temporal[IO]): Timeout[IO] = new Timeout[IO] {
    override def timeoutTo[A](fa: IO[A], after: FiniteDuration, fallback: IO[A]): IO[A] = fa.timeoutTo(after, fallback)
  }
}

trait LowPriorTimeoutImplicits { self: Timeout.type =>
  implicit def concurrent[F[_]](implicit F: Concurrent[F], timer: Temporal[F], @unused nt: NonTofu[F]): Timeout[F] =
    new Timeout[F] {
      override def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A] =
        F.race(timer.sleep(after), fa).getOrElseF(fallback)
    }
}
