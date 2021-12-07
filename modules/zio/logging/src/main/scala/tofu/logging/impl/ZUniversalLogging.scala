package tofu.logging.impl

import org.slf4j.{LoggerFactory, Marker}
import tofu.logging.{LoggedValue, Logging}
import zio._

class ZUniversalLogging(name: String) extends Logging[UIO] {
  def write(level: Logging.Level, message: String, values: LoggedValue*): UIO[Unit] =
    ZIO.effectTotal {
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.write(level, logger, message, values)
    }

  override def writeMarker(level: Logging.Level, message: String, marker: Marker, values: LoggedValue*): UIO[Unit] =
    ZIO.effectTotal {
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeMarker(level, logger, marker, message, values)
    }

  override def writeCause(level: Logging.Level, message: String, cause: Throwable, values: LoggedValue*): UIO[Unit] =
    ZIO.effectTotal {
      val logger = LoggerFactory.getLogger(name)
      if (UniversalLogging.enabled(level, logger))
        UniversalLogging.writeCause(level, logger, cause, message, values)
    }
}
