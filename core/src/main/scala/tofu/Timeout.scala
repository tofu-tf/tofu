package tofu

import cats.Applicative
import cats.syntax.option._
import cats.effect.{Concurrent, ContextShift, IO, Timer}
import simulacrum.typeclass
import tofu.syntax.feither._
import tofu.internal.NonTofu

import scala.annotation.implicitNotFound
import scala.concurrent.duration.FiniteDuration

/**
 * Timeout typeclass allow to do useful effect operation - timeoutTo.
 * Instance come with few handy syntax methods.
 * The reason this typeclass's existing - abstract over default constraints for timeouting in TF by CE: Concurrent and Timer.
 * @tparam F an effect type constructor
 */
@typeclass
@implicitNotFound("""Can't resolve instance for type constructor ${F}.
Check do you provide instances of Concurrent[${F}] and Timer[${F}] in case of TF style or
instances of ContextShift[${F}] and Timer[${F}] in case of pure IO style""")
trait Timeout[F[_]] {
  /**
   * Timeout fa with duration and return fallback in case of fa's timeout.
   * @param fa program to timeout
   * @param after duration for timeout
   * @param fallback program to return in case of fa's timeout
   * @tparam A value's type
   * @return
   */
  def timeoutTo[A](fa: F[A])(after: FiniteDuration, fallback: F[A]): F[A]

  /**
   * Timeout fa with duration and raise error in case of fa's timeout.
   * @param fa program to timeout
   * @param after duration for timeout
   * @param err value of error to raise
   * @param R Raise instance for effect F and error type E
   * @tparam A value's type
   * @tparam E error's type
   * @return
   */
  def timeoutRaise[A, E](fa: F[A])(after: FiniteDuration, err: E)(implicit R: Raise[F, E]): F[A] =
    timeoutTo(fa)(after, R.raise(err))

  /**
   * Timeout fa with duration and return pure fallback in case of fa's timeout.
   * @param fa program to timeout
   * @param after duration for timeout
   * @param fallback value to return in case of fa's timeout
   * @param F Applicative instance for effect F
   * @tparam A value's type
   * @return
   */
  def timeoutOr[A](fa: F[A])(after: FiniteDuration, fallback: A)(implicit F: Applicative[F]): F[A] =
    timeoutTo(fa)(after, F.pure(fallback))

  /**
   * Timeout fa with duration and return Some[A] is case of success and None in case of failure.
   * @param fa program to timeout
   * @param after duration for timeout
   * @param F Applicative instance for effect F
   * @tparam A value's type inside effect
   * @return
   */
  def timeout[A](fa: F[A])(after: FiniteDuration)(implicit F: Applicative[F]): F[Option[A]] =
    timeoutTo(F.map(fa)(_.some))(after, F.pure(None))
}

object Timeout extends LowPriorTimeoutImplicits {
  implicit def io(implicit timer: Timer[IO], cs: ContextShift[IO]): Timeout[IO] = new Timeout[IO] {
    override def timeoutTo[A](fa: IO[A])(after: FiniteDuration, fallback: IO[A]): IO[A] = fa.timeoutTo(after, fallback)
  }
}

trait LowPriorTimeoutImplicits { self: Timeout.type =>
  implicit def concurrent[F[_]: NonTofu](implicit F: Concurrent[F], timer: Timer[F]): Timeout[F] =
    new Timeout[F] {
      override def timeoutTo[A](fa: F[A])(after: FiniteDuration, fallback: F[A]): F[A] =
        F.race(timer.sleep(after), fa).getOrElseF(fallback)
    }
}
