package tofu.logging.zlogs
import org.slf4j.LoggerFactory
import tofu.logging.{Loggable, LoggedValue}
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
      val loggerName            = parseLoggerName(trace)
      val slf4jLogger           = LoggerFactory.getLogger(loggerName)
      val tofuLevel             = ZUniversalContextLogging.zioLevel2TofuLevel(logLevel)
      val tofuAnnotations       = context.get(ZLogContextLive.TofuLogContextRef).getOrElse(Map.empty)
      val loggedValues = tofuAnnotations.toSeq.map { case (annotation, value) =>
        annotation.apply(value.asInstanceOf[annotation.Value])
      }
      val spansCtx: LoggedValue =
        if (spans.isEmpty) ()
        else spansLoggable(jc.millis()).loggedValue(spans)

      ZUniversalContextLogging.write(
        logger = slf4jLogger,
        level = tofuLevel,
        message = message(),
        ctx = spansCtx,
        values = loggedValues :+ annotations
      )
    }
  }

}

object TofuZLogger {

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
      .singleton("spans")
      .contramap(_.map(span => (span.label, now - span.startTime)).toMap)

  private val DefaultLoggerName = "tofu.logging.zlogs.TofuZLogger"
}
