package tofu.logging

import io.chrisdavenport.log4cats._
//same
package object log4cats {
  implicit def toLog4CatsLogger[F[_]](implicit logging: Logging[F]): StructuredLogger[F] = new StructuredLogger[F] {
    override def trace(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.trace(msg, None, ctx)

    override def trace(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.traceCause(msg, None, t, ctx)

    override def debug(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.debug(msg, None, ctx)

    override def debug(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.debugCause(msg, None, t, ctx)

    override def info(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.info(msg, None, ctx)

    override def info(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.infoCause(msg, None, t, ctx)

    override def warn(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.warn(msg, None, ctx)

    override def warn(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.warnCause(msg, None, t, ctx)

    override def error(ctx: Map[String, String])(msg: => String): F[Unit] =
      logging.error(msg, None, ctx)

    override def error(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
      logging.errorCause(msg, None, t, ctx)

    override def error(message: => String): F[Unit] =
      logging.error(message, None)

    override def warn(message: => String): F[Unit] =
      logging.warn(message, None)

    override def info(message: => String): F[Unit] =
      logging.info(message, None)

    override def debug(message: => String): F[Unit] =
      logging.debug(message, None)

    override def trace(message: => String): F[Unit] =
      logging.trace(message, None)

    override def error(t: Throwable)(message: => String): F[Unit] =
      logging.errorCause(message, None, t)

    override def warn(t: Throwable)(message: => String): F[Unit] =
      logging.warnCause(message, None, t)

    override def info(t: Throwable)(message: => String): F[Unit] =
      logging.infoCause(message, None, t)

    override def debug(t: Throwable)(message: => String): F[Unit] =
      logging.debugCause(message, None, t)

    override def trace(t: Throwable)(message: => String): F[Unit] =
      logging.traceCause(message, None, t)
  }
}
