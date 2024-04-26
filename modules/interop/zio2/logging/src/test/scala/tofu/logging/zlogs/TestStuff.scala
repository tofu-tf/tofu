package tofu.logging.zlogs

import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import derevo.derive
import org.slf4j.LoggerFactory
import tofu.logging.derivation.loggable
import zio.{Clock, FiberRef, LogSpan, ULayer, ZIO, ZLayer}
import scala.jdk.CollectionConverters._

import zio._

object TestStuff {

  // ZIO 2.0.4 LogSpan uses java.lang.System for currentTime, therefore it's untestable
  def addLogSpan[R, E, A](name: String)(zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    Clock.instant.flatMap(instant =>
      FiberRef.currentLogSpan.locallyWith(list => LogSpan(name, instant.toEpochMilli) :: list)(zio)
    )
  }

  @derive(loggable)
  case class User(name: String)

  type LogAppender = ListAppender[ILoggingEvent]

  object LogAppender {
    val events: URIO[LogAppender, List[ILoggingEvent]] =
      ZIO.serviceWith[LogAppender](_.list.asScala.toList)

    def layer(loggerName: String): ULayer[LogAppender] = ZLayer.scoped {
      ZIO.acquireRelease(
        ZIO.succeed {
          val logger   = LoggerFactory.getLogger(loggerName).asInstanceOf[Logger]
          val appender = new ListAppender[ILoggingEvent]
          appender.start()
          logger.addAppender(appender)
          appender
        }
      )(a => ZIO.succeed(a.stop()))
    }
  }

}
