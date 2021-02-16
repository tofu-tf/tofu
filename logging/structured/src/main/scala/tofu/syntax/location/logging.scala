package tofu.syntax.location

import tofu.logging.{LoggedValue, LoggingBase}
import tofu.logging.location.Location
import tofu.syntax.loggable
import cats.syntax.option._
object logging {
  val braces: Array[Seq[String]] = Array.range(0, 30).map(Array.fill(_)("{}").toSeq)

  implicit final class LoggingInterpolator(private val sctx: StringContext) extends AnyVal {
    import loggable._
    def error[F[_]](values: LoggedValue*)(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.error(sctx.s(braces(values.size): _*), location.some, values: _*)
    def warn[F[_]](values: LoggedValue*)(implicit logging: LoggingBase[F], location: Location): F[Unit]  =
      logging.warn(sctx.s(braces(values.size): _*), location.some, values: _*)
    def info[F[_]](values: LoggedValue*)(implicit logging: LoggingBase[F], location: Location): F[Unit]  =
      logging.info(sctx.s(braces(values.size): _*), location.some, values: _*)
    def debug[F[_]](values: LoggedValue*)(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.debug(sctx.s(braces(values.size): _*), location.some, values: _*)
    def trace[F[_]](values: LoggedValue*)(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.trace(sctx.s(braces(values.size): _*), location.some, values: _*)

    def errorWith[F[_]](values: LoggedValue*)(
        add: (String, LoggedValue)*
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.error(sctx.s(braces(values.size): _*), location.some, values :+ add.toMap.loggedValue: _*)
    def warnWith[F[_]](values: LoggedValue*)(
        add: (String, LoggedValue)*
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.warn(sctx.s(braces(values.size): _*), location.some, values :+ add.toMap.loggedValue: _*)
    def infoWith[F[_]](values: LoggedValue*)(
        add: (String, LoggedValue)*
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.info(sctx.s(braces(values.size): _*), location.some, values :+ add.toMap.loggedValue: _*)
    def debugWith[F[_]](values: LoggedValue*)(
        add: (String, LoggedValue)*
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.debug(sctx.s(braces(values.size): _*), location.some, values :+ add.toMap.loggedValue: _*)
    def traceWith[F[_]](values: LoggedValue*)(
        add: (String, LoggedValue)*
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.trace(sctx.s(braces(values.size): _*), location.some, values :+ add.toMap.loggedValue: _*)

    def errorCause[F[_]](values: LoggedValue*)(
        ex: Throwable
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.errorCause(sctx.s(braces(values.size): _*), location.some, ex, values: _*)
    def warnCause[F[_]](values: LoggedValue*)(
        ex: Throwable
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.warnCause(sctx.s(braces(values.size): _*), location.some, ex, values: _*)
    def infoCause[F[_]](values: LoggedValue*)(
        ex: Throwable
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.infoCause(sctx.s(braces(values.size): _*), location.some, ex, values: _*)
    def debugCause[F[_]](values: LoggedValue*)(
        ex: Throwable
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.debugCause(sctx.s(braces(values.size): _*), location.some, ex, values: _*)
    def traceCause[F[_]](values: LoggedValue*)(
        ex: Throwable
    )(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.traceCause(sctx.s(braces(values.size): _*), location.some, ex, values: _*)
  }

  implicit final class LoggingCauseOps(private val message: String) extends AnyVal {
    def cause[F[_]](ex: Throwable)(implicit logging: LoggingBase[F], location: Location): F[Unit] =
      logging.errorCause(message, location.some, ex)
  }
}
