package tofu.logging
package impl

import org.slf4j.{Logger, Marker}
import tofu.logging.Logging.{Debug, Error, Info, Level, Trace, Warn}

abstract class LoggingImpl[F[_]](logger: Logger) extends Logging[F] {

  def traceEnabled: Boolean = logger.isTraceEnabled
  def debugEnabled: Boolean = logger.isDebugEnabled
  def infoEnabled: Boolean  = logger.isInfoEnabled
  def warnEnabled: Boolean  = logger.isWarnEnabled
  def errorEnabled: Boolean = logger.isErrorEnabled

  override def write(level: Level, message: String, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => trace(message)
      case Debug => debug(message)
      case Info  => info(message)
      case Warn  => warn(message)
      case Error => error(message)
    }

  override def writeMarker(level: Level, message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => traceWithMarker(message, marker)
      case Debug => debugWithMarker(message, marker)
      case Info  => infoWithMarker(message, marker)
      case Warn  => warnWithMarker(message, marker)
      case Error => errorWithMarker(message, marker)
    }

  override def writeCause(level: Level, message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => traceCause(message, cause, values: _*)
      case Debug => debugCause(message, cause, values: _*)
      case Info  => infoCause(message, cause, values: _*)
      case Warn  => warnCause(message, cause, values: _*)
      case Error => errorCause(message, cause, values: _*)
    }
}
