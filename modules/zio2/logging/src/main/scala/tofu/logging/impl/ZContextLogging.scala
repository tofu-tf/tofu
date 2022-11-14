package tofu.logging.impl

import org.slf4j.{Logger, Marker}
import tofu.logging.{Loggable, LoggedValue, Logging}
import tofu.logging.zlogs.ZLogging
import zio._

class ZContextLogging[R, Ctx: Loggable](logger: => Logger, ctxLog: URIO[R, Ctx]) extends ZLogging[R] {
  override def write(level: Logging.Level, message: String, values: LoggedValue*): URIO[R, Unit] =
    ctxLog.flatMap { ctx =>
      ZIO.succeed(
        ZUniversalContextLogging.write(level, logger, message, ctx, values, Nil)
      )
    }

  override def writeMarker(level: Logging.Level, message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    ctxLog.flatMap { ctx =>
      ZIO.succeed(
        ZUniversalContextLogging.write(level, logger, message, ctx, values, marker :: Nil)
      )
    }

  override def writeCause(level: Logging.Level, message: String, cause: Throwable, values: LoggedValue*): URIO[R, Unit] =
    ctxLog.flatMap { ctx =>
      ZIO.succeed(
        ZUniversalContextLogging.writeCause(level, logger, cause, message, ctx, values, Nil)
      )
    }
}
