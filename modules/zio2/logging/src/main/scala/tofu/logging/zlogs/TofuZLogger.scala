package tofu.logging.zlogs
import org.slf4j.LoggerFactory
import tofu.logging.{Loggable, LoggedValue}
import tofu.logging.impl.ZUniversalContextLogging
import tofu.logging.zlogs.TofuZLogger._
import zio._
import java.time.{Clock => JClock}

class TofuZLogger(logContext: ZLogContext.LoggedValuesExtractor, jc: JClock) extends ZLogger[String, Unit] {

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
      val ctx                   = logContext.loggedValueFromFiberRefs(context)
      val spansCtx: LoggedValue =
        if (spans.isEmpty) ()
        else spansLoggable(jc.millis()).loggedValue(spans)

      ZUniversalContextLogging.write(
        logger = slf4jLogger,
        level = tofuLevel,
        message = message(),
        ctx = spansCtx,
        values = Seq[LoggedValue](ctx, annotations)
      )
    }
  }

}

object TofuZLogger {

  val addToRuntime: URLayer[ZLogContext.LoggedValuesExtractor, Unit] = ZLayer(
    for {
      javaClock <- Clock.javaClock
      context   <- ZIO.service[ZLogContext.LoggedValuesExtractor]
    } yield Runtime.addLogger(new TofuZLogger(context, javaClock))
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
