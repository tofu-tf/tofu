package tofu.logging.impl

import org.slf4j.{LoggerFactory, Marker}
import tofu.logging.{LoggedValue, Logging}
import zio._

class ZUniversalContextLogging[R](name: String, fctx: (LoggedValue => Unit) => URIO[R, Unit])
    extends Logging[URIO[R, *]] {
  def write(level: Logging.Level, message: String, values: LoggedValue*): URIO[R, Unit] =
    fctx { ctx =>
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeMarker(level, logger, ContextMarker(ctx), message, values)
    }

  override def writeMarker(level: Logging.Level, message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    fctx { ctx =>
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeMarker(level, logger, ContextMarker(ctx, List(marker)), message, values)
    }
}
