package tofu.logging
package impl

import cats.effect.Sync
import org.slf4j.{ILoggerFactory, Logger, Marker}
import tofu.logging.location.Location
import tofu.syntax.monadic._
class SyncLogging[F[_]](val logger: Logger, factory: ILoggerFactory)(implicit F: Sync[F]) extends LoggingImpl[F] {

  private[tofu] def log(l: Logger => Unit, location: Option[Location], when: Logger => Boolean) =
    location match {
      case Some(location) =>
        for {
          lg <- F.delay(factory.getLogger(location.loggerName)) //we should also use other fields from location
          _  <- F.delay(l(lg)).whenA(when(lg))
        } yield ()
      case None           => F.delay(l(logger)).whenA(when(logger))
    }


  override def trace(message: String, location: Option[Location], values: LoggedValue*): F[Unit] =
    log(_.trace(message, values: _*), location, traceEnabled)
  override def debug(message: String, location: Option[Location], values: LoggedValue*): F[Unit] =
    log(_.debug(message, values: _*), location, debugEnabled)
  override def info(message: String, location: Option[Location], values: LoggedValue*): F[Unit]  =
    log(_.info(message, values: _*), location, infoEnabled)
  override def warn(message: String, location: Option[Location], values: LoggedValue*): F[Unit]  =
    log(_.warn(message, values: _*), location, warnEnabled)
  override def error(message: String, location: Option[Location], values: LoggedValue*): F[Unit] =
    log(_.error(message, values: _*), location, errorEnabled)

  override def traceWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit] =
    log(_.trace(marker, message, values: _*), location, traceEnabled)
  override def debugWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit] =
    log(_.debug(marker, message, values: _*), location, debugEnabled)
  override def infoWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit] =
    log(_.info(marker, message, values: _*), location, infoEnabled)
  override def warnWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit] =
    log(_.warn(marker, message, values: _*), location, warnEnabled)
  override def errorWithMarker(
      message: String,
      location: Option[Location],
      marker: Marker,
      values: LoggedValue*
  ): F[Unit] =
    log(_.error(marker, message, values: _*), location, errorEnabled)

  override def errorCause(
      message: String,
      location: Option[Location],
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    log(_.error(message, values :+ cause: _*), location, errorEnabled)
  override def warnCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    log(_.warn(message, values :+ cause: _*), location, warnEnabled)
  override def infoCause(message: String, location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    log(_.info(message, values :+ cause: _*), location, infoEnabled)
  override def debugCause(
      message: String,
      location: Option[Location],
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    log(_.debug(message, values :+ cause: _*), location, debugEnabled)
  override def traceCause(
      message: String,
      location: Option[Location],
      cause: Throwable,
      values: LoggedValue*
  ): F[Unit]                                                                                                           =
    log(_.trace(message, values :+ cause: _*), location, traceEnabled)
  
}
