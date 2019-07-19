package tofu.logging
package impl

import cats.effect.Sync
import cats.syntax.applicative._
import org.slf4j.Logger

class SyncLogging[F[_]](logger: Logger)(implicit F: Sync[F]) extends LoggingImpl[F](logger) {
  override def trace(message: String, values: LoggedValue*) =
    F.delay(logger.trace(message, values: _*)).whenA(traceEnabled)
  override def debug(message: String, values: LoggedValue*) =
    F.delay(logger.debug(message, values: _*)).whenA(debugEnabled)
  override def info(message: String, values: LoggedValue*) =
    F.delay(logger.info(message, values: _*)).whenA(infoEnabled)
  override def warn(message: String, values: LoggedValue*) =
    F.delay(logger.warn(message, values: _*)).whenA(warnEnabled)
  override def error(message: String, values: LoggedValue*) =
    F.delay(logger.error(message, values: _*)).whenA(errorEnabled)

  override def errorCause(message: String, cause: Throwable): F[Unit] =
    F.delay(logger.error(message, cause)).whenA(errorEnabled)
  override def warnCause(message: String, cause: Throwable): F[Unit] =
    F.delay(logger.warn(message, cause)).whenA(errorEnabled)
  override def infoCause(message: String, cause: Throwable): F[Unit] =
    F.delay(logger.info(message, cause)).whenA(errorEnabled)
  override def debugCause(message: String, cause: Throwable): F[Unit] =
    F.delay(logger.debug(message, cause)).whenA(errorEnabled)
  override def traceCause(message: String, cause: Throwable): F[Unit] =
    F.delay(logger.trace(message, cause)).whenA(errorEnabled)
}
