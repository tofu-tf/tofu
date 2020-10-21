package tofu.logging.scribe.internal

import org.slf4j.helpers.FormattingTuple
import org.slf4j.helpers.MessageFormatter.{arrayFormat => arrFmt, format => fmt}
import org.slf4j.{Logger, Marker}

import scribe.Level._
import scribe.{LazyMessage, Level, LogRecord, Logger => ScribeLogger}

class ScribeLoggerAdapter(name: String) extends Logger {
  def getName: String = name


  def isTraceEnabled: Boolean = includes(Trace)

  def isTraceEnabled(marker: Marker): Boolean = isTraceEnabled

  def trace(msg: String): Unit = log(Trace, msg, None)

  def trace(format: String, arg: Any): Unit = log(Trace, fmt(format, arg))

  def trace(format: String, arg1: Any, arg2: Any): Unit = log(Trace, fmt(format, arg1, arg2))

  def trace(format: String, args: AnyRef*): Unit = log(Trace, arrFmt(format, args.toArray))

  def trace(msg: String, err: Throwable): Unit = log(Trace, msg, Option(err))

  def trace(marker: Marker, msg: String): Unit = log(marker, Trace, msg)

  def trace(marker: Marker, msg: String, err: Throwable): Unit = log(marker, Trace, msg, err = Option(err))

  def trace(marker: Marker, format: String, arg: Any): Unit =
    log(marker, Trace, fmt(format, arg))

  def trace(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    log(marker, Trace, fmt(format, arg1, arg2))

  def trace(marker: Marker, format: String, args: AnyRef*): Unit =
    log(marker, Trace, arrFmt(format, args.toArray))


  def isDebugEnabled: Boolean = includes(Debug)

  def isDebugEnabled(marker: Marker): Boolean = isDebugEnabled

  def debug(msg: String): Unit = log(Debug, msg, None)

  def debug(format: String, arg: Any): Unit = log(Debug, fmt(format, arg))

  def debug(format: String, arg1: Any, arg2: Any): Unit = log(Debug, fmt(format, arg1, arg2))

  def debug(format: String, args: AnyRef*): Unit = log(Debug, arrFmt(format, args.toArray))

  def debug(msg: String, err: Throwable): Unit = log(Debug, msg, Option(err))

  def debug(marker: Marker, msg: String): Unit = log(marker, Debug, msg)

  def debug(marker: Marker, msg: String, err: Throwable): Unit = log(marker, Debug, msg, err = Option(err))

  def debug(marker: Marker, format: String, arg: Any): Unit =
    log(marker, Debug, fmt(format, arg))

  def debug(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    log(marker, Debug, fmt(format, arg1, arg2))

  def debug(marker: Marker, format: String, args: AnyRef*): Unit =
    log(marker, Debug, arrFmt(format, args.toArray))


  def isInfoEnabled: Boolean = includes(Info)

  def isInfoEnabled(marker: Marker): Boolean = isInfoEnabled

  def info(msg: String): Unit = log(Info, msg, None)

  def info(format: String, arg: Any): Unit = log(Info, fmt(format, arg))

  def info(format: String, arg1: Any, arg2: Any): Unit = log(Info, fmt(format, arg1, arg2))

  def info(format: String, args: AnyRef*): Unit = log(Info, arrFmt(format, args.toArray))

  def info(msg: String, err: Throwable): Unit = log(Info, msg, Option(err))

  def info(marker: Marker, msg: String): Unit = log(marker, Info, msg)

  def info(marker: Marker, msg: String, err: Throwable): Unit = log(marker, Info, msg, err = Option(err))

  def info(marker: Marker, format: String, arg: Any): Unit =
    log(marker, Info, fmt(format, arg))

  def info(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    log(marker, Info, fmt(format, arg1, arg2))

  def info(marker: Marker, format: String, args: AnyRef*): Unit =
    log(marker, Info, arrFmt(format, args.toArray))


  def isWarnEnabled: Boolean = includes(Warn)

  def isWarnEnabled(marker: Marker): Boolean = isWarnEnabled

  def warn(msg: String): Unit = log(Warn, msg, None)

  def warn(format: String, arg: Any): Unit = log(Warn, fmt(format, arg))

  def warn(format: String, arg1: Any, arg2: Any): Unit = log(Warn, fmt(format, arg1, arg2))

  def warn(format: String, args: AnyRef*): Unit = log(Warn, arrFmt(format, args.toArray))

  def warn(msg: String, err: Throwable): Unit = log(Warn, msg, Option(err))

  def warn(marker: Marker, msg: String): Unit = log(marker, Warn, msg)

  def warn(marker: Marker, msg: String, err: Throwable): Unit = log(marker, Warn, msg, err = Option(err))

  def warn(marker: Marker, format: String, arg: Any): Unit =
    log(marker, Warn, fmt(format, arg))

  def warn(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    log(marker, Warn, fmt(format, arg1, arg2))

  def warn(marker: Marker, format: String, args: AnyRef*): Unit =
    log(marker, Warn, arrFmt(format, args.toArray))


  def isErrorEnabled: Boolean = includes(Error)

  def isErrorEnabled(marker: Marker): Boolean = isErrorEnabled

  def error(msg: String): Unit = log(Error, msg, None)

  def error(format: String, arg: Any): Unit = log(Error, fmt(format, arg))

  def error(format: String, arg1: Any, arg2: Any): Unit = log(Error, fmt(format, arg1, arg2))

  def error(format: String, args: AnyRef*): Unit = log(Error, arrFmt(format, args.toArray))

  def error(msg: String, err: Throwable): Unit = log(Error, msg, Option(err))

  def error(marker: Marker, msg: String): Unit = log(marker, Error, msg)

  def error(marker: Marker, msg: String, err: Throwable): Unit = log(marker, Error, msg, err = Option(err))

  def error(marker: Marker, format: String, arg: Any): Unit =
    log(marker, Error, fmt(format, arg))

  def error(marker: Marker, format: String, arg1: Any, arg2: Any): Unit =
    log(marker, Error, fmt(format, arg1, arg2))

  def error(marker: Marker, format: String, args: AnyRef*): Unit =
    log(marker, Error, arrFmt(format, args.toArray))


  private[this] final def includes(lvl: Level): Boolean = ScribeLogger(name).includes(lvl)

  private[this] final def log(lvl: Level, msg: String, err: Option[Throwable]): Unit = {
    import scribe.Loggable.StringLoggable

    ScribeLogger(name).log(LogRecord(
      level      = lvl,
      value      = lvl.value,
      message    = new LazyMessage(() => msg),
      loggable   = StringLoggable,
      throwable  = err,
      fileName   = "",
      className  = name,
      methodName = None,
      line       = None,
      column     = None
    ))
  }

  private[this] final def log(lvl: Level, tuple: FormattingTuple): Unit =
    log(lvl, tuple.getMessage, Option(tuple.getThrowable))

  private[this] final def log(
    marker: Marker,
    lvl: Level,
    msg: String,
    args: Array[AnyRef] = Array.emptyObjectArray,
    err: Option[Throwable] = None
  ): Unit = ScribeLogger(name).log(LogRecord(
    level      = lvl,
    value      = lvl.value,
    message    = new LazyMessage(() => MarkedOutput(marker, msg, args)),
    loggable   = MarkedOutput.NonBoxingLoggable,
    throwable  = err,
    fileName   = "",
    className  = name,
    methodName = None,
    line       = None,
    column     = None
  ))

  private[this] final def log(marker: Marker, lvl: Level, tuple: FormattingTuple): Unit =
    log(marker, lvl, tuple.getMessage, tuple.getArgArray, Option(tuple.getThrowable))
}