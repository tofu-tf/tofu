package tofu
package logging
package impl

import cats.Applicative
import cats.syntax.applicative._
import org.slf4j.{Logger, Marker}
import tofu.logging.location.Location

import scala.reflect.ClassTag
//we should use location here too, but for now I am lazy to do it.
//I will wait for design approval
class ContextLoggingImpl[F[_]: Applicative, C: Loggable, Service: ClassTag](context: F HasContext C, logger: Logger)
    extends LoggingImpl[F] {

  override def trace(message: String, location: Option[Location], values: LoggedValue*) =
    context.ask(ctx => logger.trace(ContextMarker(ctx), message, values: _*)).whenA(traceEnabled(logger))
  override def debug(message: String,location: Option[Location], values: LoggedValue*) =
    context.ask(ctx => logger.debug(ContextMarker(ctx), message, values: _*)).whenA(debugEnabled(logger))
  override def info(message: String,location: Option[Location], values: LoggedValue*)  =
    context.ask(ctx => logger.info(ContextMarker(ctx), message, values: _*)).whenA(infoEnabled(logger))
  override def warn(message: String,location: Option[Location],values: LoggedValue*)  =
    context.ask(ctx => logger.warn(ContextMarker(ctx), message, values: _*)).whenA(warnEnabled(logger))
  override def error(message: String, location: Option[Location],values: LoggedValue*) =
    context.ask(ctx => logger.error(ContextMarker(ctx), message, values: _*)).whenA(errorEnabled(logger))

  override def traceWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    context.ask(ctx => logger.trace(ContextMarker(ctx).addMarker(marker), message, values: _*)).whenA(traceEnabled(logger))
  override def debugWithMarker(message: String, location: Option[Location],marker: Marker, values: LoggedValue*): F[Unit] =
    context.ask(ctx => logger.debug(ContextMarker(ctx).addMarker(marker), message, values: _*)).whenA(debugEnabled(logger))
  override def infoWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit]  =
    context.ask(ctx => logger.info(ContextMarker(ctx).addMarker(marker), message, values: _*)).whenA(infoEnabled(logger))
  override def warnWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit]  =
    context.ask(ctx => logger.warn(ContextMarker(ctx).addMarker(marker), message, values: _*)).whenA(warnEnabled(logger))
  override def errorWithMarker(message: String,location: Option[Location], marker: Marker, values: LoggedValue*): F[Unit] =
    context.ask(ctx => logger.error(ContextMarker(ctx).addMarker(marker), message, values: _*)).whenA(errorEnabled(logger))

  override def traceCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    context.ask(ctx => logger.trace(ContextMarker(ctx), message, values :+ cause: _*)).whenA(traceEnabled(logger))
  override def debugCause(message: String,location: Option[Location], cause: Throwable, values: LoggedValue*): F[Unit] =
    context.ask(ctx => logger.debug(ContextMarker(ctx), message, values :+ cause: _*)).whenA(debugEnabled(logger))
  override def infoCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): F[Unit]  =
    context.ask(ctx => logger.info(ContextMarker(ctx), message, values :+ cause: _*)).whenA(infoEnabled(logger))
  override def warnCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): F[Unit]  =
    context.ask(ctx => logger.error(ContextMarker(ctx), message, values :+ cause: _*)).whenA(warnEnabled(logger))
  override def errorCause(message: String, location: Option[Location],cause: Throwable, values: LoggedValue*): F[Unit] =
    context.ask(ctx => logger.error(ContextMarker(ctx), message, values :+ cause: _*)).whenA(errorEnabled(logger))
}
