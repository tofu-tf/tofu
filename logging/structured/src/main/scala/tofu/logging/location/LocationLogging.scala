package tofu.logging.location

import cats.effect.Sync
import org.slf4j.{LoggerFactory, Marker}
import tofu.compat.`package`.unused
import tofu.logging.Logging.{Debug, Error, Info, Level, Trace, Warn}
import tofu.logging.impl.{ContextSyncLoggingImpl, SyncLogging}
import tofu.logging.{LoggableContext, LoggedValue, Logging, Logs}
import tofu.syntax.monadic._

case class Location(className: String, line: Int) {
  def loggerName: String = className
}

trait LocationLogging[F[_]] {
  def writeLocation(level: Logging.Level, location: Location, message: String, values: LoggedValue*): F[Unit]

  def writeMarker(
      level: Level,
      message: String,
      @unused marker: Marker,
      location: Location,
      values: LoggedValue*
  ): F[Unit] =
    writeLocation(level, location: Location, message, values: _*)

  /** could be overridden in the implementations, write message about some exception */
  def writeCause(level: Level, location: Location, message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeLocation(level, location, message, values :+ LoggedValue.error(cause): _*)

  def trace(message: String, location: Location, values: LoggedValue*): F[Unit] =
    writeLocation(Trace, location, message, values: _*)
  def debug(message: String, location: Location, values: LoggedValue*): F[Unit] =
    writeLocation(Debug, location, message, values: _*)
  def info(message: String, location: Location, values: LoggedValue*): F[Unit]  =
    writeLocation(Info, location, message, values: _*)
  def warn(message: String, location: Location, values: LoggedValue*): F[Unit]  =
    writeLocation(Warn, location, message, values: _*)
  def error(message: String, location: Location, values: LoggedValue*): F[Unit] =
    writeLocation(Error, location, message, values: _*)

  def traceWithMarker(message: String, location: Location, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Trace, message, marker, location, values: _*)
  def debugWithMarker(message: String, location: Location, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Debug, message, marker, location, values: _*)
  def infoWithMarker(message: String, location: Location, marker: Marker, values: LoggedValue*): F[Unit]  =
    writeMarker(Info, message, marker, location, values: _*)
  def warnWithMarker(message: String, location: Location, marker: Marker, values: LoggedValue*): F[Unit]  =
    writeMarker(Warn, message, marker, location, values: _*)
  def errorWithMarker(message: String, location: Location, marker: Marker, values: LoggedValue*): F[Unit] =
    writeMarker(Error, message, marker, location, values: _*)

  def traceCause(message: String, location: Location, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Trace, location, message, cause, values: _*)
  def debugCause(message: String, location: Location, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Debug, location, message, cause, values: _*)
  def infoCause(message: String, location: Location, cause: Throwable, values: LoggedValue*): F[Unit]  =
    writeCause(Info, location, message, cause, values: _*)
  def warnCause(message: String, location: Location, cause: Throwable, values: LoggedValue*): F[Unit]  =
    writeCause(Warn, location, message, cause, values: _*)
  def errorCause(message: String, location: Location, cause: Throwable, values: LoggedValue*): F[Unit] =
    writeCause(Error, location, message, cause, values: _*)

}

object LocationLogging {
  def sync[I[_]: Sync, F[_]: Sync]: I[LocationLogging[F]] =
    Sync[I].delay(LoggerFactory.getILoggerFactory).map { factory =>
      new LocationLogging[F] {
        override def writeLocation(level: Logging.Level, location: Location, message: String, values: LoggedValue*)
            : F[Unit] =
          for {
            logger <- Sync[F].delay(factory.getLogger(location.loggerName)) //cached inside factory
            logging = new SyncLogging[F](logger)
            _      <- logging.write(level, message, values: _*)
          } yield ()
      }
    }

  def withContext[I[_]: Sync, F[_]: Sync](implicit ctx: LoggableContext[F]): I[LocationLogging[F]] = {
    import ctx.loggable

    Sync[I].delay(LoggerFactory.getILoggerFactory).map { factory =>
      new LocationLogging[F] {
        override def writeLocation(level: Logging.Level, location: Location, message: String, values: LoggedValue*)
            : F[Unit] =
          for {
            logger <- Sync[F].delay(factory.getLogger(location.loggerName)) //cached inside factory
            logging = new ContextSyncLoggingImpl[F, ctx.Ctx](ctx.context, logger)
            _      <- logging.write(level, message, values: _*)
          } yield ()
      }
    }
  }
}
