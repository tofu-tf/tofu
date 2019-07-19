package tofu
package logging

import java.time.Instant

import cats.syntax.monoid._
import ch.qos.logback.classic.PatternLayout
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.CoreConstants.{LINE_SEPARATOR => EOL}
import tofu.data.PArray
import impl.ContextMarker
import syntax.logRenderer._

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

  val builder = TethysBuilder(postfix = EOL)

  implicit val iLoggingEventLoggable: Loggable[ILoggingEvent] = new DictLoggable[ILoggingEvent]
  with ToStringLoggable[ILoggingEvent] {
    def fields[I, V, R, M](evt: ILoggingEvent, i: I)(implicit rec: LogRenderer[I, V, R, M]): R = {
      val default =
        rec.addString(TimeStampField, Instant.ofEpochMilli(evt.getTimeStamp).toString, i) |+|
          rec.addString(LoggerNameField, evt.getLoggerName, i) |+|
          rec.addString(ThreadNameField, evt.getThreadName, i) |+|
          rec.addString(LevelField, evt.getLevel.toString, i) |+|
          rec.addString(MessageField, evt.getFormattedMessage.trim, i)

      val fromMarker = evt.getMarker match {
        case null                         => i.noop
        case ContextMarker(marker, Seq()) => marker.logFields(i)
        case ContextMarker(marker, rest) =>
          import PArray.arrInstance // scala 2.11
          val restArr = PArray.fromColl(rest)
          marker.logFields(i) |+|
            i.sub(MarkersField)((v: V) => v.foldable(restArr)(_ putString _.getName))
        case marker => i.sub(MarkersField)((v: V) => v.list(1)((v, _) => v.putString(marker.getName)))
      }

      val arguments = evt.getArgumentArray match {
        case null => i.noop
        case array =>
          array.foldLeft(fromMarker)((acc, arg) =>
            acc |+| (arg match {
              case lv: LoggedValue => lv.logFields(i)
              case _               => i.noop
            }))
      }

      val exception = evt.getThrowableProxy match {
        case null => i.noop
        case throwableProxy =>
          i.addString(ExceptionField, s"${throwableProxy.getClassName}: ${throwableProxy.getMessage}") |+| {
            val stackTrace = throwableProxy.getStackTraceElementProxyArray
            if (stackTrace.isEmpty) i.noop else i.addString(StackTraceField, stackTrace.mkString(EOL))
          }
      }

      default |+| fromMarker |+| arguments |+| exception
    }
  }
}
