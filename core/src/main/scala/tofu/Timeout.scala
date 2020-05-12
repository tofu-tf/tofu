package tofu

import cats.effect.{Concurrent, ContextShift, IO, Timer}
import simulacrum.typeclass
import cats.syntax.apply._
import cats.syntax.functor._

import scala.concurrent.duration.FiniteDuration

@typeclass
trait Timeout[F[_]] {
  def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A]
}

object Timeout {
  implicit def concurrent[F[_]](implicit F: Concurrent[F], timer: Timer[F]): Timeout[F] =
    new Timeout[F] {
      override def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A] =
        F.race(fa, timer.sleep(after) *> fallback).map(_.merge)
    }

  implicit def io(implicit timer: Timer[IO], cs: ContextShift[IO]): Timeout[IO] = new Timeout[IO] {
    override def timeoutTo[A](fa: IO[A], after: FiniteDuration, fallback: IO[A]): IO[A] = fa.timeoutTo(after, fallback)
  }
}
