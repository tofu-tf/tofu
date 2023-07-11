package tofu.syntax

import cats.Applicative
import tofu.time.Timeout
import raise.FindRaise

import scala.concurrent.duration.FiniteDuration

object timeout {
  implicit final class TimeoutAddinitionalOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def timeoutTo(after: FiniteDuration, fallback: F[A])(implicit timeout: Timeout[F]): F[A] =
      timeout.timeoutTo(fa, after, fallback)

    def timeoutRaise[E](after: FiniteDuration, err: E)(implicit timeout: Timeout[F], raise: FindRaise.Aux[E, F]): F[A] =
      timeout.timeoutTo(fa, after, FindRaise.unwrap(raise).raise(err))

    def timeoutOr(after: FiniteDuration, fallback: A)(implicit timeout: Timeout[F], app: Applicative[F]): F[A] =
      timeout.timeoutTo(fa, after, app.pure(fallback))

    def timeout(after: FiniteDuration)(implicit timeout: Timeout[F], app: Applicative[F]): F[Option[A]] =
      timeout.timeoutTo(app.map(fa)(Some(_)), after, app.pure(None))
  }
}
