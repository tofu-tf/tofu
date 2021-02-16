package tofu.logging.zlogs.impl

import tofu.logging.{Loggable, LoggedValue}
import org.slf4j.{Logger, Marker}
import tofu.logging.impl.{ContextMarker, LoggingImpl}
import tofu.logging.location.Location
import zio.{UIO, URIO}
//will use location here after design approval
class URIOZLoggingImpl[R: Loggable](logger: Logger) extends LoggingImpl[URIO[R, *]] {
  override def trace(message: String,location: Option[Location], values: LoggedValue*) =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.trace(ContextMarker(ctx), message, values: _*))).when(traceEnabled(logger))
  override def debug(message: String,location: Option[Location], values: LoggedValue*) =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.debug(ContextMarker(ctx), message, values: _*))).when(debugEnabled(logger))
  override def info(message: String,location: Option[Location], values: LoggedValue*)  =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.info(ContextMarker(ctx), message, values: _*))).when(infoEnabled(logger))
  override def warn(message: String,location: Option[Location], values: LoggedValue*)  =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.warn(ContextMarker(ctx), message, values: _*))).when(warnEnabled(logger))
  override def error(message: String,location: Option[Location], values: LoggedValue*) =
    URIO.accessM[R](ctx => UIO.effectTotal(logger.error(ContextMarker(ctx), message, values: _*))).when(errorEnabled(logger))

  override def traceWithMarker(message: String, location: Option[Location],marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.trace(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(traceEnabled(logger))
  override def debugWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.debug(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(debugEnabled(logger))
  override def infoWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.info(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(infoEnabled(logger))
  override def warnWithMarker(message: String, location: Option[Location],marker: Marker, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.warn(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(warnEnabled(logger))
  override def errorWithMarker(message: String, location: Option[Location],marker: Marker, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.error(ContextMarker(ctx).addMarker(marker), message, values: _*)))
      .when(errorEnabled(logger))

  override def traceCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.trace(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(traceEnabled(logger))
  override def debugCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.debug(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(debugEnabled(logger))
  override def infoCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.info(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(infoEnabled(logger))
  override def warnCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): URIO[R, Unit]  =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.warn(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(warnEnabled(logger))
  override def errorCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): URIO[R, Unit] =
    URIO
      .accessM[R](ctx => UIO.effectTotal(logger.error(ContextMarker(ctx), message, values :+ cause: _*)))
      .when(errorEnabled(logger))
}
