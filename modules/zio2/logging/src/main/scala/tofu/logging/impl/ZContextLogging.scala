package tofu.logging.impl

import org.slf4j.{Logger, Marker}
import tofu.logging.{Loggable, LoggedValue, Logging}
import tofu.logging.zlogs.ZLogging
import zio._

class ZContextLogging[R, C: Loggable](logger: Logger, ctxLog: URIO[R, C]) extends ZLogging[R] {
  def write(level: Logging.Level, message: String, values: LoggedValue*): URIO[R, Unit] =
    ctxLog.flatMap { ctx =>
      ZIO.succeed(
        ZUniversalContextLogging.write(logger, level, message, ctx, values, Nil)
      )
    }

  override def writeMarker(level: Logging.Level, message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    ctxLog.flatMap { ctx =>
      ZIO.succeed(
        ZUniversalContextLogging.write(logger, level, message, ctx, values, marker :: Nil)
      )
    }
}
