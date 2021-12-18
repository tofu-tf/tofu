package tofu.logging

import cats.Applicative
import tofu.logging.Logging.Level

private[tofu] class EmptyLogging[F[_] : Applicative] extends Logging[F] {
  private[this] val noop = Applicative[F].unit

  def write(level: Level, message: String, values: LoggedValue*): F[Unit] = noop

  override def asLogging: Logging[F] = this
}
