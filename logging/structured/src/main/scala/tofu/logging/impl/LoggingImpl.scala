package tofu.logging
package impl

import org.slf4j.{Logger, Marker}
import tofu.logging.Logging.{Debug, Error, Info, Level, Trace, Warn}
import tofu.logging.location.Location

/* */
private[tofu] abstract class LoggingImpl[F[_]] extends Logging[F] {


  def traceEnabled(logger:Logger): Boolean = logger.isTraceEnabled
  def debugEnabled(logger:Logger): Boolean = logger.isDebugEnabled
  def infoEnabled(logger:Logger): Boolean = logger.isInfoEnabled
  def warnEnabled(logger:Logger): Boolean = logger.isWarnEnabled
  def errorEnabled(logger:Logger): Boolean = logger.isErrorEnabled

  override def write(level: Level,      location: Option[Location], message: String, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => trace(message,  location, values: _*)
      case Debug => debug(message,location, values: _*)
      case Info  => info(message, location,values: _*)
      case Warn  => warn(message, location,values: _*)
      case Error => error(message,location, values: _*)
    }

  override def writeMarker(level: Level,     location: Option[Location], message: String, marker: Marker, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => traceWithMarker(message, location,marker, values: _*)
      case Debug => debugWithMarker(message, location,marker, values: _*)
      case Info  => infoWithMarker(message, location,marker, values: _*)
      case Warn  => warnWithMarker(message,location, marker, values: _*)
      case Error => errorWithMarker(message, location,marker, values: _*)
    }

  override def writeCause(level: Level,      location: Option[Location],message: String, cause: Throwable, values: LoggedValue*): F[Unit] =
    level match {
      case Trace => traceCause(message,location, cause, values: _*)
      case Debug => debugCause(message,location, cause, values: _*)
      case Info  => infoCause(message, location,cause, values: _*)
      case Warn  => warnCause(message, location,cause, values: _*)
      case Error => errorCause(message, location,cause, values: _*)
    }
}
