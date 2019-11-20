package tofu
package logging

import ch.qos.logback.classic.PatternLayout
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.CoreConstants.{LINE_SEPARATOR => EOL}
import tofu.logging.logback.EventLoggable

/** logging layout writing JSON receivable by logstash */
class ELKLayout extends PatternLayout {
  import ELKLayout.iLoggingEventLoggable

  override def doLayout(event: ILoggingEvent): String = ELKLayout.builder(event)
}

object ELKLayout {

  val MarkersField    = "markers"
  val TimeStampField  = "@timestamp"
  val LoggerNameField = "loggerName"
  val ThreadNameField = "threadName"
  val LevelField      = "level"
  val HostNameField   = "hostName"
  val MessageField    = "message"
  val MessageIdField  = "messageId"
  val ExceptionField  = "exception"
  val StackTraceField = "stackTrace"

  val builder: TethysBuilder = TethysBuilder(postfix = EOL)

  implicit val iLoggingEventLoggable: Loggable[ILoggingEvent] = EventLoggable.default
}


