package tofu.logging.scribe

import cats.syntax.monoid._

import tofu.logging.{DictLoggable, LogRenderer, Loggable, ToStringLoggable}
import tofu.logging.ELKFormatter._
import tofu.logging.scribe.RecordLoggable.LogRecord
import tofu.syntax.logRenderer._

trait RecordLoggable extends DictLoggable[LogRecord] with ToStringLoggable[LogRecord]

class RecordBuiltInLoggable extends RecordLoggable {
  import java.time.Instant
  import scribe.format.FormatBlock

  def fields[I, V, R, S](rec: LogRecord, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
    i.addString(TimeStampField, Instant.ofEpochMilli(rec.timeStamp).toString) |+|
    i.addString(LoggerNameField, FormatBlock.Position.format(rec).plainText) |+|
    i.addString(ThreadNameField, rec.thread.getName) |+|
    i.addString(LevelField, rec.level.name) |+|
    i.addString(MessageField, rec.loggable(rec.message.value).plainText.trim)
}

class RecordMarkerLoggable extends RecordLoggable {
  import tofu.logging.impl.ContextMarker
  import tofu.logging.scribe.internal.MarkedOutput

  def fields[I, V, R, S](rec: LogRecord, i: I)(implicit r: LogRenderer[I, V, R, S]): R = rec match {
    case MarkedOutput(marker, _, _) =>
      marker match {
        case ContextMarker(marker, Nil) =>
          marker.logFields(i)
        case ContextMarker(marker, rest) =>
          import tofu.data.PArray
          import PArray.arrInstance

          val restArr = PArray.fromColl(rest)
          marker.logFields(i) |+| i.sub(MarkersField)((v: V) => v.foldable(restArr)(_ putString _.getName))
        case marker =>
          i.sub(MarkersField)((v: V) => v.list(1)((v, _) => v.putString(marker.getName)))
      }
    case _ => i.noop
  }
}

class RecordArgumentsLoggable extends RecordLoggable {
  import tofu.logging.scribe.internal.MarkedOutput
  import tofu.logging.LoggedValue

  def fields[I, V, R, S](rec: LogRecord, i: I)(implicit r: LogRenderer[I, V, R, S]): R = rec match {
    case MarkedOutput(_, _, args) if args.nonEmpty =>
      args.foldLeft(i.noop)((acc, arg) =>
        acc |+| (arg match {
          case v: LoggedValue => v.logFields(i)
          case _              => i.noop
        })
      )
    case _ => i.noop
  }
}

class RecordExceptionLoggable extends RecordLoggable {
  def fields[I, V, R, S](rec: LogRecord, i: I)(implicit r: LogRenderer[I, V, R, S]): R = rec.throwable match {
    case Some(ex) =>
      i.addString(ExceptionField, ex.getMessage match {
        case null => ex.getClass.getName
        case msg  => s"${ex.getClass.getName}: $msg"
      }) |+|
      i.addString(StackTraceField, format(ex.getStackTrace))
    case _        => i.noop
  }

  private[this] final def format(trace: Array[StackTraceElement]): String =
    if (trace.isEmpty) ""
    else {
      val builder = new StringBuilder()

      for (elem <- trace) {
        import elem._

        builder ++= s"\tat $getClassName.$getMethodName("
        builder ++= (getLineNumber match {
          case -2         => "Native Method"
          case n if n > 0 => s"$getFileName:$n"
          case _          => getFileName
        })
        builder ++= ")\n"
      }

      builder.result()
    }
}

object RecordLoggable {
  type LogRecord = scribe.LogRecord[Any]

  lazy val builtIn: Loggable[LogRecord]   = new RecordBuiltInLoggable
  lazy val marker: Loggable[LogRecord]    = new RecordMarkerLoggable
  lazy val arguments: Loggable[LogRecord] = new RecordArgumentsLoggable
  lazy val exception: Loggable[LogRecord] = new RecordExceptionLoggable

  lazy val merge: Loggable[LogRecord] = builtIn + marker + arguments + exception
}