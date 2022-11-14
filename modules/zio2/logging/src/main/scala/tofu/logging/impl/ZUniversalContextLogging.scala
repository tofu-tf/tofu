package tofu.logging.impl

import org.slf4j.{Logger, LoggerFactory, Marker}
import tofu.logging._
import zio.URIO

class ZUniversalContextLogging[R, Ctx: Loggable](name: String, ctxLog: URIO[R, Ctx])
  extends ZContextLogging[R, Ctx](LoggerFactory.getLogger(name), ctxLog)

object ZUniversalContextLogging {

  private[logging] def write(
      level: Logging.Level,
      logger: Logger,
      message: => String,
      ctx: LoggedValue,
      values: Seq[LoggedValue],
      markers: List[Marker] = Nil
  ): Unit = {
    if (UniversalLogging.enabled(level, logger))
      UniversalLogging.writeMarker(
        level = level,
        logger = logger,
        marker = ContextMarker(ctx, markers),
        message = message,
        values = values
      )
  }

  private[logging] def writeCause(
      level: Logging.Level,
      logger: Logger,
      cause: Throwable,
      message: => String,
      ctx: LoggedValue,
      values: Seq[LoggedValue],
      markers: List[Marker] = Nil
  ): Unit = {
    if (UniversalLogging.enabled(level, logger))
      UniversalLogging.writeMarkerCause(
        level = level,
        logger = logger,
        marker = ContextMarker(ctx, markers),
        cause = cause,
        message = message,
        values = values
      )
  }

}
