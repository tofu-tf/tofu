package tofu.logging.zlogs.impl

import tofu.logging.{Loggable, LoggedValue}
import org.slf4j.{Logger, Marker}
import tofu.logging.impl.{ContextMarker, LoggingImpl}
import zio.{UIO, URIO}

class URIOZLoggingImpl[R: Loggable](logger: Logger) extends LoggingImpl[URIO[R, *]](logger) {
  override def trace(message: String, values: LoggedValue*) =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.trace(ContextMarker(ctx), message, values: _*))).when(traceEnabled)
  override def debug(message: String, values: LoggedValue*) =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.debug(ContextMarker(ctx), message, values: _*))).when(debugEnabled)
  override def info(message: String, values: LoggedValue*)  =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.info(ContextMarker(ctx), message, values: _*))).when(infoEnabled)
  override def warn(message: String, values: LoggedValue*)  =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.warn(ContextMarker(ctx), message, values: _*))).when(warnEnabled)
  override def error(message: String, values: LoggedValue*) =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.error(ContextMarker(ctx), message, values: _*))).when(errorEnabled)

  override def traceWithMarker(message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.trace(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(traceEnabled)
  override def debugWithMarker(message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.debug(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(debugEnabled)
  override def infoWithMarker(message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.info(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(infoEnabled)
  override def warnWithMarker(message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.warn(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(warnEnabled)
  override def errorWithMarker(message: String, marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.error(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(errorEnabled)

  override def traceCause(message: String, cause: Throwable, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.trace(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(traceEnabled)
  override def debugCause(message: String, cause: Throwable, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.debug(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(debugEnabled)
  override def infoCause(message: String, cause: Throwable, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.info(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(infoEnabled)
  override def warnCause(message: String, cause: Throwable, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.warn(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(warnEnabled)
  override def errorCause(message: String, cause: Throwable, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.error(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(errorEnabled)
}
