package tofu.logging.zlogs.impl

import org.slf4j.Logger
import tofu.logging.LoggedValue
import tofu.logging.impl.LoggingImpl
import zio.UIO

class UIOZLogging(logger: Logger) extends LoggingImpl[UIO](logger) {
  override def trace(message: String, values: LoggedValue*) =
    UIO.effectTotal(logger.trace(message, values: _*)).when(traceEnabled)
  override def debug(message: String, values: LoggedValue*) =
    UIO.effectTotal(logger.debug(message, values: _*)).when(debugEnabled)
  override def info(message: String, values: LoggedValue*)  =
    UIO.effectTotal(logger.info(message, values: _*)).when(infoEnabled)
  override def warn(message: String, values: LoggedValue*)  =
    UIO.effectTotal(logger.warn(message, values: _*)).when(warnEnabled)
  override def error(message: String, values: LoggedValue*) =
    UIO.effectTotal(logger.error(message, values: _*)).when(errorEnabled)

  override def errorCause(message: String, cause: Throwable, values: LoggedValue*): UIO[Unit] =
    UIO.effectTotal(logger.error(message, values :+ cause: _*)).when(errorEnabled)
  override def warnCause(message: String, cause: Throwable, values: LoggedValue*): UIO[Unit]  =
    UIO.effectTotal(logger.warn(message, values :+ cause: _*)).when(warnEnabled)
  override def infoCause(message: String, cause: Throwable, values: LoggedValue*): UIO[Unit]  =
    UIO.effectTotal(logger.info(message, values :+ cause: _*)).when(infoEnabled)
  override def debugCause(message: String, cause: Throwable, values: LoggedValue*): UIO[Unit] =
    UIO.effectTotal(logger.debug(message, values :+ cause: _*)).when(debugEnabled)
  override def traceCause(message: String, cause: Throwable, values: LoggedValue*): UIO[Unit] =
    UIO.effectTotal(logger.trace(message, values :+ cause: _*)).when(traceEnabled)
}
