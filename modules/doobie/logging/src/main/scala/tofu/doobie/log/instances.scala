package tofu.doobie.log

import cats.syntax.monoid._
import doobie.util.log.{ExecFailure, LogEvent, ProcessingFailure, Success}
import tofu.logging.{DictLoggable, LogRenderer, Loggable, LoggedValue}
import tofu.syntax.logRenderer._

object instances {
  implicit val logEventLoggable: Loggable[LogEvent] = new DictLoggable[LogEvent] {
    private def oneline(s: String): String                          =
      s.linesIterator.map(_.trim).filterNot(_.isEmpty).mkString(" ")
    private def multiline(s: String, indent: String = "  "): String =
      s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n" + indent)

    private def loggedArgs(args: List[Any]): LoggedValue =
      args.collect { case x: LoggedValue =>
        x
      }

    def logShow(ev: LogEvent): String = ev match {
      case Success(s, a, e1, e2)              =>
        s"""Successful Statement Execution:
           |
           |  ${multiline(s)}
           |
           | arguments = ${loggedArgs(a)}
           |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (${(e1 + e2).toMillis} ms total)
          """.stripMargin
      case ProcessingFailure(s, a, e1, e2, t) =>
        s"""Failed Resultset Processing:
           |
           |  ${multiline(s)}
           |
           | arguments = ${loggedArgs(a)}
           |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (failed) (${(e1 + e2).toMillis} ms total)
           |   failure = ${t.getMessage}
          """.stripMargin
      case ExecFailure(s, a, e1, t)           =>
        s"""Failed Statement Execution:
           |
           |  ${multiline(s)}
           |
           | arguments = ${loggedArgs(a)}
           |   elapsed = ${e1.toMillis} ms exec (failed)
           |   failure = ${t.getMessage}
          """.stripMargin
    }

    def fields[I, V, R, S](ev: LogEvent, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      ev match {
        case Success(s, a, e1, e2)              =>
          i.field("sql-event-type", "Success") |+|
            i.field("sql-statement", oneline(s)) |+|
            i.field("sql-args", loggedArgs(a)) |+|
            i.field("sql-exec-ms", e1.toMillis) |+|
            i.field("sql-processing-ms", e2.toMillis) |+|
            i.field("sql-total-ms", (e1 + e2).toMillis)
        case ProcessingFailure(s, a, e1, e2, _) =>
          i.field("sql-event-type", "ProcessingFailure") |+|
            i.field("sql-statement", oneline(s)) |+|
            i.field("sql-args", loggedArgs(a)) |+|
            i.field("sql-exec-ms", e1.toMillis) |+|
            i.field("sql-processing-ms", e2.toMillis) |+|
            i.field("sql-total-ms", (e1 + e2).toMillis)
        case ExecFailure(s, a, e1, _)           =>
          i.field("sql-event-type", "ExecFailure") |+|
            i.field("sql-statement", oneline(s)) |+|
            i.field("sql-args", loggedArgs(a)) |+|
            i.field("sql-exec-ms", e1.toMillis)
      }
  }
}
