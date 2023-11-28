package tofu.logging
package impl

import cats.FlatMap
import org.slf4j.{Logger, LoggerFactory, Marker}
import tofu.logging.Logging.{Debug, Error, Info, Trace, Warn}
import tofu.{Delay, WithContext}
import scala.annotation.nowarn
import scala.collection.immutable.Seq

@nowarn("msg=a type was inferred to be `Object`") //cat=lint-infer-any filter does not work for Scala 3
object UniversalLogging {
  private[impl] final def enabled(level: Logging.Level, logger: Logger): Boolean = level match {
    case Trace => logger.isTraceEnabled()
    case Debug => logger.isDebugEnabled()
    case Info  => logger.isInfoEnabled()
    case Warn  => logger.isWarnEnabled()
    case Error => logger.isErrorEnabled()
  }

  private[impl] final def write(level: Logging.Level, logger: Logger, message: String, values: Seq[LoggedValue]): Unit =
    level match {
      case Trace => logger.trace(message, values: _*)
      case Debug => logger.debug(message, values: _*)
      case Info  => logger.info(message, values: _*)
      case Warn  => logger.warn(message, values: _*)
      case Error => logger.error(message, values: _*)
    }

  private[impl] final def writeMarker(
      level: Logging.Level,
      logger: Logger,
      marker: Marker,
      message: String,
      values: Seq[LoggedValue]
  ): Unit =
    level match {
      case Trace => logger.trace(marker, message, values: _*)
      case Debug => logger.debug(marker, message, values: _*)
      case Info  => logger.info(marker, message, values: _*)
      case Warn  => logger.warn(marker, message, values: _*)
      case Error => logger.error(marker, message, values: _*)
    }

  private[impl] final def writeCause(
      level: Logging.Level,
      logger: Logger,
      cause: Throwable,
      message: String,
      values: Seq[LoggedValue]
  ): Unit =
    level match {
      case Trace => logger.trace(message, values :+ cause: _*)
      case Debug => logger.debug(message, values :+ cause: _*)
      case Info  => logger.info(message, values :+ cause: _*)
      case Warn  => logger.warn(message, values :+ cause: _*)
      case Error => logger.error(message, values :+ cause: _*)
    }

  private[impl] final def writeMarkerCause(
      level: Logging.Level,
      logger: Logger,
      marker: Marker,
      cause: Throwable,
      message: String,
      values: Seq[LoggedValue]
  ): Unit =
    level match {
      case Trace => logger.trace(marker, message, values :+ cause: _*)
      case Debug => logger.debug(marker, message, values :+ cause: _*)
      case Info  => logger.info(marker, message, values :+ cause: _*)
      case Warn  => logger.warn(marker, message, values :+ cause: _*)
      case Error => logger.error(marker, message, values :+ cause: _*)
    }
}

class UniversalLogging[F[_]](name: String)(implicit F: Delay[F]) extends Logging[F] {
  def write(level: Logging.Level, message: String, values: LoggedValue*): F[Unit] =
    F.delay {
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.write(level, logger, message, values)
    }

  override def writeMarker(level: Logging.Level, message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    F.delay {
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeMarker(level, logger, marker, message, values)
    }

  override def writeCause(level: Logging.Level, message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    F.delay {
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeCause(level, logger, cause, message, values)
    }
}

class UniversalContextLogging[F[_]](name: String, fctx: (LoggedValue => Unit) => F[Unit]) extends Logging[F] {
  def write(level: Logging.Level, message: String, values: LoggedValue*): F[Unit] =
    fctx { ctx =>
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeMarker(level, logger, ContextMarker(ctx), message, values)
    }

  override def writeCause(level: Logging.Level, message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    fctx { ctx =>
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeMarkerCause(level, logger, ContextMarker(ctx), cause, message, values)
    }

  override def writeMarker(level: Logging.Level, message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    fctx { ctx =>
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeMarker(level, logger, ContextMarker(ctx, List(marker)), message, values)
    }
}

class UniversalContextLogs[F[_]: FlatMap, C: Loggable](implicit FC: F WithContext C, FD: Delay[F])
    extends Logs.Universal[F] {
  private def useContextValue(f: LoggedValue => Unit): F[Unit] = FC.askF(c => FD.delay(f(c)))
  override def byName(name: String): Logging[F]                = new UniversalContextLogging[F](name, useContextValue)
}
