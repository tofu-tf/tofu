package tofu.logging.zlogs.impl

import org.slf4j.Logger
import tofu.logging.LoggedValue
import tofu.logging.impl.LoggingImpl
import tofu.logging.location.Location
import zio.UIO

class UIOZLogging(logger: Logger) extends LoggingImpl[UIO] {
  override def trace(message: String,location: Option[Location], values: LoggedValue*) =
    UIO.effectTotal(logger.trace(message, values: _*)).when(traceEnabled(logger))
  override def debug(message: String, location: Option[Location],values: LoggedValue*) =
    UIO.effectTotal(logger.debug(message, values: _*)).when(debugEnabled(logger))
  override def info(message: String,location: Option[Location], values: LoggedValue*)  =
    UIO.effectTotal(logger.info(message, values: _*)).when(infoEnabled(logger))
  override def warn(message: String, location: Option[Location],values: LoggedValue*)  =
    UIO.effectTotal(logger.warn(message, values: _*)).when(warnEnabled(logger))
  override def error(message: String,location: Option[Location], values: LoggedValue*) =
    UIO.effectTotal(logger.error(message, values: _*)).when(errorEnabled(logger))

  override def errorCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): UIO[Unit] =
    UIO.effectTotal(logger.error(message, values :+ cause: _*)).when(errorEnabled(logger))
  override def warnCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): UIO[Unit]  =
    UIO.effectTotal(logger.warn(message, values :+ cause: _*)).when(warnEnabled(logger))
  override def infoCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): UIO[Unit]  =
    UIO.effectTotal(logger.info(message, values :+ cause: _*)).when(infoEnabled(logger))
  override def debugCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): UIO[Unit] =
    UIO.effectTotal(logger.debug(message, values :+ cause: _*)).when(debugEnabled(logger))
  override def traceCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): UIO[Unit] =
    UIO.effectTotal(logger.trace(message, values :+ cause: _*)).when(traceEnabled(logger))
}
