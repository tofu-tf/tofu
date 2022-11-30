package tofu.logging.zlogs
import org.slf4j.{Logger, LoggerFactory}
import tofu.logging.impl.TofuDefaultWithZIOContextImpl.makeZioContext
import tofu.logging.impl.{ComposedLoggedValue, ZUniversalContextLogging}
import tofu.logging.zlogs.TofuZLogger._
import tofu.logging.{LoggedValue, Logging}
import zio.Cause.{Die, Fail}
import zio._

import java.time.{Clock => JClock}

class TofuZLogger(jc: JClock) extends ZLogger[String, Unit] {

  protected val zioLevel2TofuLevel: LogLevel => Logging.Level = {
    case LogLevel.All     => Logging.Trace
    case LogLevel.Trace   => Logging.Trace
    case LogLevel.Debug   => Logging.Debug
    case LogLevel.Info    => Logging.Info
    case LogLevel.Warning => Logging.Warn
    case LogLevel.Error   => Logging.Error
    case LogLevel.Fatal   => Logging.Error
    case _                => Logging.Info // shouldn't happen
  }

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
      val tofuLevel                = zioLevel2TofuLevel(logLevel)
      val tofuContext              = context
        .get(TofuDefaultContext.AnnotatedContextRef)
        .getOrElse(Map.empty)
      val loggerName               = TofuDefaultContext
        .getValueUnsafe(ZLogAnnotation.loggerName)(tofuContext)
        .getOrElse(parseLoggerName(trace))
      val slf4jLogger              = LoggerFactory.getLogger(loggerName)
      val values: Seq[LoggedValue] =
        tofuContext.map { case (k, v) =>
          k -> v.asInstanceOf[k.Value]
        }.toSeq
      val ctx                      = new ComposedLoggedValue(makeZioContext(jc.millis(), annotations, spans))
      writeLog(tofuLevel, slf4jLogger, message, ctx, values, cause)
    }
  }

}

object TofuZLogger {

  /** Adds the [[TofuZLogger]] to your application's runtime.
    *
    * @example
    *   {{{
    * zioProgram.provide(
    *   Runtime.removeDefaultLoggers >>> TofuZLogger.addToRuntime
    * )
    *   }}}
    */
  val addToRuntime: ULayer[Unit] = ZLayer(
    Clock.javaClock.map(jc => Runtime.addLogger(new TofuZLogger(jc)))
  ).flatten

  /** Parses logger name from [[Trace]]
    *
    * Trace value ''tofu.example.logging.ZioLoggingNativeExample$.run(ZioLoggingNativeExample.scala:15)'' will be parsed
    * as ''tofu.example.logging.ZioLoggingNativeExample$''
    */
  def parseLoggerName(trace: Trace): String = trace match {
    case Trace(location, _, _) =>
      val last = location.lastIndexOf(".")
      if (last > 0) location.substring(0, last)
      else location
    case _                     => DefaultLoggerName
  }

  private val DefaultLoggerName = "tofu.logging.zlogs.TofuZLogger"

  private def writeLog(
      level: Logging.Level,
      logger: Logger,
      message: () => String,
      ctx: LoggedValue,
      values: Seq[LoggedValue],
      cause: Cause[Any]
  ): Unit = cause.find {
    case Die(th, _)             => th
    case Fail(th: Throwable, _) => th
  }.fold(
    ZUniversalContextLogging.write(level, logger, message(), ctx, values)
  )(th => ZUniversalContextLogging.writeCause(level, logger, th, message(), ctx, values))
}
