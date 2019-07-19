package tofu.syntax

import cats.Applicative
import tofu.{Raise, Timeout}

import scala.concurrent.duration.FiniteDuration

object timeout extends Timeout.ToTimeoutOps {
  implicit class TimeoutAddinitionalOps[F[_], A](val fa: F[A]) extends AnyVal {
    def timeoutRaise[E](after: FiniteDuration, err: E)(implicit timeout: Timeout[F], raise: Raise[F, E]): F[A] =
      timeout.timeoutTo(fa, after, raise.raise(err))

    def timeoutOr(after: FiniteDuration, fallback: A)(implicit timeout: Timeout[F], app: Applicative[F]): F[A] =
      timeout.timeoutTo(fa, after, app.pure(fallback))

    def timeout(after: FiniteDuration)(implicit timeout: Timeout[F], app: Applicative[F]): F[Option[A]] =
      timeout.timeoutTo(app.map(fa)(Some(_)), after, app.pure(None))
  }
}
