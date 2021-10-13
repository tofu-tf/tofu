package tofu.logging
package impl

import org.slf4j.{Logger, Marker}
import tofu.logging.Logging.{Debug, Error, Info, Level, Trace, Warn}

/* */
private[tofu] abstract class LoggingImpl[F[_]](logger: Logger) extends Logging[F] {

  def traceEnabled: Boolean = logger.isTraceEnabled
  def debugEnabled: Boolean = logger.isDebugEnabled
  def infoEnabled: Boolean  = logger.isInfoEnabled
  def warnEnabled: Boolean  = logger.isWarnEnabled
  def errorEnabled: Boolean = logger.isErrorEnabled

  override def write(level: Level, message: String, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => trace(message, values: _*)
      case Debug => debug(message, values: _*)
      case Info  => info(message, values: _*)
      case Warn  => warn(message, values: _*)
      case Error => error(message, values: _*)
    }

  override def writeMarker(level: Level, message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => traceWithMarker(message, marker, values: _*)
      case Debug => debugWithMarker(message, marker, values: _*)
      case Info  => infoWithMarker(message, marker, values: _*)
      case Warn  => warnWithMarker(message, marker, values: _*)
      case Error => errorWithMarker(message, marker, values: _*)
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
