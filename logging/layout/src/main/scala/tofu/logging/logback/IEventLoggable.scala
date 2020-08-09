package tofu
package logging
package logback

import java.time.Instant

import cats.syntax.monoid._
import ch.qos.logback.classic.spi.{ILoggingEvent, ThrowableProxyUtil}
import impl.ContextMarker
import syntax.logRenderer._
import tofu.data.PArray
import tofu.logging.ELKLayout._

trait EventLoggable extends DictLoggable[ILoggingEvent] with ToStringLoggable[ILoggingEvent]

class EventBuiltInLoggable extends EventLoggable {
  def fields[I, V, R, S](evt: ILoggingEvent, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
    r.addString(TimeStampField, Instant.ofEpochMilli(evt.getTimeStamp).toString, i) |+|
      r.addString(LoggerNameField, evt.getLoggerName, i) |+|
      r.addString(ThreadNameField, evt.getThreadName, i) |+|
      r.addString(LevelField, evt.getLevel.toString, i) |+|
      r.addString(MessageField, evt.getFormattedMessage.trim, i)
}

class EventMarkerLoggable extends EventLoggable {
  def fields[I, V, R, S](evt: ILoggingEvent, i: I)(implicit rec: LogRenderer[I, V, R, S]): R =
    evt.getMarker match {
      case null                         => i.noop
      case ContextMarker(marker, Seq()) => marker.logFields(i)
      case ContextMarker(marker, rest)  =>
        import PArray.arrInstance // scala 2.11
        val restArr = PArray.fromColl(rest)
        marker.logFields(i) |+|
          i.sub(MarkersField)((v: V) => v.foldable(restArr)(_ putString _.getName))
      case marker                       => i.sub(MarkersField)((v: V) => v.list(1)((v, _) => v.putString(marker.getName)))
    }
}

class EventArgumentsLoggable extends EventLoggable {
  def fields[I, V, R, S](evt: ILoggingEvent, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
    evt.getArgumentArray match {
      case null  => i.noop
      case array =>
        array.foldLeft(i.noop)((acc, arg) =>
          acc |+| (arg match {
            case lv: LoggedValue => lv.logFields(i)
            case _               => i.noop
          })
        )
    }
}

class EventArgumentsArrayLoggable(fieldName: String) extends EventLoggable {
  def fields[I, V, R, S](evt: ILoggingEvent, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
    evt.getArgumentArray match {
      case null  => i.noop
      case array =>
        i.subDictList(fieldName, array.length)((inner, idx) =>
          array(idx) match {
            case lv: LoggedValue => lv.logFields(inner)
            case _               => inner.noop
          }
        )
    }
}

class EventArgumentsGroupLoggable extends EventLoggable {
  def fields[I, V, R, S](evt: ILoggingEvent, i: I)(implicit render: LogRenderer[I, V, R, S]): R =
    evt.getArgumentArray match {
      case null  => i.noop
      case array =>
        array.collect { case lv: LoggedValue => lv }
          .groupBy(_.shortName)
          .foldLeft(i.noop) {
            case (r, ("", values))         => values.foldLeft(r)(_ |+| _.logFields(i))
            case (r, (name, Array(value))) => r |+| i.subDict(name)(value.logFields(_))
            case (r, (name, values))       => r |+| i.subDictList(name, values.length)((si, idx) => values(idx).logFields(si))
          }
    }
}

class EventExceptionLoggable extends EventLoggable {
  def fields[I, V, R, S](evt: ILoggingEvent, i: I)(implicit rec: LogRenderer[I, V, R, S]): R =
    evt.getThrowableProxy match {
      case null           => i.noop
      case throwableProxy =>
        i.addString(ExceptionField, s"${throwableProxy.getClassName}: ${throwableProxy.getMessage}") |+|
          i.addString(StackTraceField, ThrowableProxyUtil.asString(throwableProxy))
    }
}

object EventLoggable {
  val builtin: Loggable[ILoggingEvent]                         = new EventBuiltInLoggable
  val marker: Loggable[ILoggingEvent]                          = new EventMarkerLoggable
  val arguments: Loggable[ILoggingEvent]                       = new EventArgumentsLoggable
  def argumentArray(argField: String): Loggable[ILoggingEvent] = new EventArgumentsArrayLoggable(argField)
  val exception: Loggable[ILoggingEvent]                       = new EventExceptionLoggable
  val argumentGroup: Loggable[ILoggingEvent]                   = new EventArgumentsGroupLoggable

  val merge: Loggable[ILoggingEvent]                     = builtin + marker + arguments + exception
  def collect(argField: String): Loggable[ILoggingEvent] =
    builtin + marker + argumentArray(argField) + exception
  val group                                              = builtin + marker + argumentGroup + exception
}
