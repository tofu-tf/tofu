package tofu.logging.zlogs
import org.slf4j.LoggerFactory
import tofu.logging.{Loggable, LoggedValue, Logging}
import tofu.logging.impl.ZUniversalContextLogging
import tofu.logging.zlogs.TofuZLogger._
import zio._

import java.time.{Clock => JClock}

class TofuZLogger(jc: JClock) extends ZLogger[String, Unit] {

  override def apply(
      trace: Trace,
      fiberId: FiberId,
      logLevel: LogLevel,
      message: () => String,
      cause: Cause[Any],
      context: FiberRefs,
      spans: List[LogSpan],
      annotations: Map[String, String]
  ): Unit = {
    if (logLevel != LogLevel.None) {
      val loggerName               = parseLoggerName(trace)
      val slf4jLogger              = LoggerFactory.getLogger(loggerName)
      val tofuLevel                = zioLevel2TofuLevel(logLevel)
      val values: Seq[LoggedValue] = context
        .get(TofuAnnotatedContextRef)
        .getOrElse(Map.empty)
        .map { case (k, v) =>
          k -> v.asInstanceOf[k.Value]
        }
        .toSeq

      val ctx: Map[String, LoggedValue] = {
        Seq.empty[(String, LoggedValue)] ++
          Option.when(annotations.nonEmpty)("zAnnotations" -> (annotations: LoggedValue)) ++
          Option.when(spans.nonEmpty)("zSpans" -> spansLoggable(jc.millis()).loggedValue(spans))
      }.toMap

      (cause.failureOption ++ cause.dieOption).collectFirst { case th: Throwable =>
        th
      } match {
        case Some(th) =>
          ZUniversalContextLogging.writeCause(
            logger = slf4jLogger,
            level = tofuLevel,
            cause = th,
            message = message(),
            ctx = ctx,
            values = values
          )
        case _        =>
          ZUniversalContextLogging.write(
            logger = slf4jLogger,
            level = tofuLevel,
            message = message(),
            ctx = ctx,
            values = values
          )
      }
    }
  }

}

object TofuZLogger {

  val zioLevel2TofuLevel: LogLevel => Logging.Level = {
    case LogLevel.All     => Logging.Trace
    case LogLevel.Trace   => Logging.Trace
    case LogLevel.Debug   => Logging.Debug
    case LogLevel.Info    => Logging.Info
    case LogLevel.Warning => Logging.Warn
    case LogLevel.Error   => Logging.Error
    case LogLevel.Fatal   => Logging.Error
    case _                => Logging.Info // shouldn't happen
  }

  val addToRuntime: ULayer[Unit] = ZLayer(
    Clock.javaClock.map(jc => Runtime.addLogger(new TofuZLogger(jc)))
  ).flatten

  def parseLoggerName(trace: Trace): String = trace match {
    case Trace(location, _, _) =>
      val last = location.lastIndexOf(".")
      if (last > 0) location.substring(0, last)
      else location
    case _                     => DefaultLoggerName
  }

  def spansLoggable(now: Long): Loggable[List[LogSpan]] =
    Loggable[Map[String, Long]]
      .contramap(_.map(span => (span.label, now - span.startTime)).toMap)

  private val DefaultLoggerName = "tofu.logging.zlogs.TofuZLogger"
}
