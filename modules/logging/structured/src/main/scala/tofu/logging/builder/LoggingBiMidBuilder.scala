package tofu.logging
package builder

import tofu.control.Bind
import tofu.logging.Logging.Level
import tofu.logging.bi.LoggingBiMid
import tofu.logging.impl.ArgsLoggable
import tofu.syntax.bindInv._

import scala.collection.mutable.Buffer
import scala.reflect.ClassTag

trait BiBuilder[+T[_, _]] {
  def prepare[Alg[_[_, _]]](implicit Alg: ClassTag[Alg[({ type L[_, _] = Any })#L]]): BiPrepared[Alg, T]
}

trait BiMethod[U[f[_, _]], Err, Res, +T[_, _]] {
  def arg[A: Loggable](name: String, a: A): BiMethod[U, Err, Res, T]

  def result: T[Err, Res]
}

trait BiPrepared[U[f[_, _]], +T[_, _]] {
  def start[Err: Loggable, Res: Loggable](method: String): BiMethod[U, Err, Res, T]
}

abstract class LoggingBiMidBuilder extends BiBuilder[LoggingBiMid] {

  /** do some logging upon enter to method invocation */
  def onEnter[F[+_, +_]: Logging.SafeBase](
      cls: Class[?],
      method: String,
      args: Seq[(String, LoggedValue)]
  ): F[Nothing, Unit]

  /** do some logging after leaving method invocation with known result or error */
  def onLeave[F[+_, +_]: Logging.SafeBase](
      cls: Class[?],
      method: String,
      args: Seq[(String, LoggedValue)],
      res: LoggedValue,
      ok: Boolean
  ): F[Nothing, Unit]

  def prepare[Alg[_[_, _]]](implicit Alg: ClassTag[Alg[({ type L[_, _] = Any })#L]]) =
    new PreparedImpl[Alg](Alg.runtimeClass)

  protected class MethodImpl[U[f[_, _]], Err: Loggable, Res: Loggable](
      cls: Class[?],
      method: String,
      args: Buffer[(String, LoggedValue)]
  ) extends BiMethod[U, Err, Res, LoggingBiMid] {
    def arg[A: Loggable](name: String, a: A) = {
      args += (name -> a)
      this
    }

    def result: LoggingBiMid[Err, Res] = new LoggingBiMid[Err, Res] {
      private[this] val argSeq = args.toSeq

      def around[F[+_, +_]: Bind: Logging.SafeBase](fa: F[Err, Res]): F[Err, Res] =
        onEnter(cls, method, argSeq) *>
          fa.tapBoth(
            err => onLeave(cls, method, argSeq, err, ok = false),
            res => onLeave(cls, method, argSeq, res, ok = true)
          )
    }
  }

  class PreparedImpl[U[f[_, _]]](cls: Class[?]) extends BiPrepared[U, LoggingBiMid] {
    def start[Err: Loggable, Res: Loggable](method: String) = new MethodImpl[U, Err, Res](cls, method, Buffer())
  }
}

object LoggingBiMidBuilder {

  class CustomLogLevel(logLevel: Level, errorLogLevel: Level) extends LoggingBiMidBuilder {

    override def onEnter[F[+_, +_]](cls: Class[?], method: String, args: Seq[(String, LoggedValue)])(implicit
        F: Logging.SafeBase[F]
    ): F[Nothing, Unit] = F.write(logLevel, "entering {} {}", method, new ArgsLoggable(args))

    override def onLeave[F[+_, +_]](
        cls: Class[?],
        method: String,
        args: Seq[(String, LoggedValue)],
        res: LoggedValue,
        ok: Boolean
    )(implicit
        F: Logging.SafeBase[F]
    ): F[Nothing, Unit] = if (ok)
      F.write(logLevel, "leaving {} {} result is {}", method, new ArgsLoggable(args), res)
    else
      F.write(errorLogLevel, "error during {} {} error is {}", method, new ArgsLoggable(args), res)
  }
  class Default extends CustomLogLevel(Logging.Debug, Logging.Error)

}
