package tofu.logging

import io.chrisdavenport.log4cats._

package object log4cats {
  implicit def toLog4CatsLogger[F[_]](implicit logging: Logging[F]): StructuredLogger[F] = new StructuredLogger[F] {
    override def trace(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.trace(msg, ctx)

    override def trace(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.traceCause(msg, t, ctx)

    override def debug(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.debug(msg, ctx)

    override def debug(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.debugCause(msg, t, ctx)

    override def info(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.info(msg, ctx)

    override def info(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.infoCause(msg, t, ctx)

    override def warn(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.warn(msg, ctx)

    override def warn(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.warnCause(msg, t, ctx)

    override def error(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.error(msg, ctx)

    override def error(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.errorCause(msg, t, ctx)

    override def error(message: => String): F[Unit] =
      logging.error(message)

    override def warn(message: => String): F[Unit] =
      logging.warn(message)

    override def info(message: => String): F[Unit] =
      logging.info(message)

    override def debug(message: => String): F[Unit] =
      logging.debug(message)

    override def trace(message: => String): F[Unit] =
      logging.trace(message)

    override def error(t: Throwable)(message: => String): F[Unit] =
      logging.errorCause(message, t)

    override def warn(t: Throwable)(message: => String): F[Unit] =
      logging.warnCause(message, t)

    override def info(t: Throwable)(message: => String): F[Unit] =
      logging.infoCause(message, t)

    override def debug(t: Throwable)(message: => String): F[Unit] =
      logging.debugCause(message, t)

    override def trace(t: Throwable)(message: => String): F[Unit] =
      logging.traceCause(message, t)
  }
}
