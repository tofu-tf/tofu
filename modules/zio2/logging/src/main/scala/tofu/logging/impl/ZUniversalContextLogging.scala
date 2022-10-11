package tofu.logging.impl

import org.slf4j.{Logger, Marker}
import zio.LogLevel
import tofu.logging._

object ZUniversalContextLogging {
  val zioLevel2TofuLevel: LogLevel => Logging.Level = {
    case LogLevel.All     => Logging.Trace
    case LogLevel.Trace   => Logging.Trace
    case LogLevel.Debug   => Logging.Debug
    case LogLevel.Info    => Logging.Info
    case LogLevel.Warning => Logging.Warn
    case LogLevel.Error   => Logging.Error
    case LogLevel.Fatal   => Logging.Error
    case _                => Logging.Info // shouldn't happen
  }

  private[logging] def write(
      logger: Logger,
      level: Logging.Level,
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

}
