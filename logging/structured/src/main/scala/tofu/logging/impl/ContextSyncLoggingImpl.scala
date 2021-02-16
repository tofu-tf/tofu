package tofu
package logging
package impl

import cats.effect.Sync
import org.slf4j.Marker
import tofu.logging.location.Location

class ContextSyncLoggingImpl[F[_], C: Loggable](context: F HasContext C, syncLogging: SyncLogging[F])(implicit F: Sync[F])
    extends LoggingImpl[F] {

  override def trace(message: String,  location: Option[Location],values: LoggedValue*) =
    context.askF(ctx => syncLogging.log(_.trace(ContextMarker(ctx), message, values: _*), location, traceEnabled))
  override def debug(message: String,  location: Option[Location],values: LoggedValue*) =
    context.askF(ctx => syncLogging.log(_.debug(ContextMarker(ctx), message, values: _*), location, debugEnabled))
  override def info(message: String,  location: Option[Location],values: LoggedValue*)  =
    context.askF(ctx => syncLogging.log(_.info(ContextMarker(ctx), message, values: _*), location, infoEnabled))
  override def warn(message: String,  location: Option[Location],values: LoggedValue*)  =
    context.askF(ctx => syncLogging.log(_.warn(ContextMarker(ctx), message, values: _*), location, warnEnabled))
  override def error(message: String, location: Option[Location], values: LoggedValue*) =
    context.askF(ctx => syncLogging.log(_.error(ContextMarker(ctx), message, values: _*), location, errorEnabled))

  override def traceWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    context
      .askF(ctx => syncLogging.log(_.trace(ContextMarker(ctx).addMarker(marker), message, values: _*), location, traceEnabled))
  override def debugWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    context
      .askF(ctx => syncLogging.log(_.debug(ContextMarker(ctx).addMarker(marker), message, values: _*), location,debugEnabled))
  override def infoWithMarker(message: String, location: Option[Location],marker: Marker, values: LoggedValue*): F[Unit]  =
    context
      .askF(ctx => syncLogging.log(_.info(ContextMarker(ctx).addMarker(marker), message, values: _*),location, infoEnabled))
  override def warnWithMarker(message: String, location: Option[Location],marker: Marker, values: LoggedValue*): F[Unit]  =
    context
      .askF(ctx => syncLogging.log(_.warn(ContextMarker(ctx).addMarker(marker), message, values: _*), location,warnEnabled))
  override def errorWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    context
      .askF(ctx => syncLogging.log(_.error(ContextMarker(ctx).addMarker(marker), message, values: _*), location, errorEnabled))

  override def traceCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    context.askF(ctx => syncLogging.log(_.trace(ContextMarker(ctx), message, values :+ cause: _*), location, traceEnabled))
  override def debugCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    context.askF(ctx => syncLogging.log(_.debug(ContextMarker(ctx), message, values :+ cause: _*), location, debugEnabled))
  override def infoCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit]  =
    context.askF(ctx => syncLogging.log(_.info(ContextMarker(ctx), message, values :+ cause: _*), location, infoEnabled))
  override def warnCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit]  =
    context.askF(ctx => syncLogging.log(_.warn(ContextMarker(ctx), message, values :+ cause: _*), location, warnEnabled))
  override def errorCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    context.askF(ctx => syncLogging.log(_.error(ContextMarker(ctx), message, values :+ cause: _*), location, errorEnabled))
}
