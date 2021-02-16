package tofu
package logging
package impl

import cats.FlatMap
import org.slf4j.Marker
import tofu.logging.location.Location
import tofu.syntax.monadic._

class EmbedLogging[F[_]: FlatMap](underlying: F[Logging[F]]) extends Logging[F] {
  def write(level: Logging.Level, location: Option[Location], message: String, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.write(level, location, message, values: _*))

  override def writeMarker(
      level: Logging.Level,
      location: Option[Location],
      message: String,
      marker: Marker,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.writeMarker(level, location, message, marker, values: _*))
  override def writeCause(
      level: Logging.Level,
      location: Option[Location],
      message: String,
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.writeCause(level, location, message, cause, values: _*))
  override def trace(message: String, location: Option[Location], values: LoggedValue*): F[Unit]                       =
    underlying.flatMap(_.trace(message, location, values: _*))
  override def debug(message: String, location: Option[Location], values: LoggedValue*): F[Unit]                       =
    underlying.flatMap(_.debug(message, location, values: _*))
  override def info(message: String, location: Option[Location], values: LoggedValue*): F[Unit]                        =
    underlying.flatMap(_.info(message, location, values: _*))
  override def warn(message: String, location: Option[Location], values: LoggedValue*): F[Unit]                        =
    underlying.flatMap(_.warn(message, location, values: _*))
  override def error(message: String, location: Option[Location], values: LoggedValue*): F[Unit]                       =
    underlying.flatMap(_.error(message, location, values: _*))
  override def traceWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.traceWithMarker(message, location, marker, values: _*))
  override def debugWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.debugWithMarker(message, location, marker, values: _*))
  override def infoWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.infoWithMarker(message, location, marker, values: _*))
  override def warnWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.warnWithMarker(message, location, marker, values: _*))
  override def errorWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.errorWithMarker(message, location, marker, values: _*))
  override def traceCause(
      message: String,
      location: Option[Location],
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.traceCause(message, location, cause, values: _*))
  override def debugCause(
      message: String,
      location: Option[Location],
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.debugCause(message, location, cause, values: _*))
  override def infoCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.infoCause(message, location, cause, values: _*))
  override def warnCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    underlying.flatMap(_.warnCause(message, location, cause, values: _*))
  override def errorCause(
      message: String,
      location: Option[Location],
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    underlying.flatMap(_.errorCause(message, location, cause, values: _*))
}
