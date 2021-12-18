package tofu.logging
package builder
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag

import tofu.control.Bind
import tofu.logging.bi.LoggingBiMid
import tofu.logging.impl.ArgsLoggable
import tofu.logging.{Loggable, LoggedValue, Logging}
import tofu.syntax.bindInv._

trait BiBuilder[+T[_, _]] {
  def prepare[Alg[_[_, _]]](implicit Alg: ClassTag[Alg[Any]]): BiPrepared[Alg, T]
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
  def onEnter[F[+_, +_]: Logging.Safe](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)]
  ): F[Nothing, Unit]

  /** do some logging after leaving method invocation with known result or error */
  def onLeave[F[+_, +_]: Logging.Safe](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)],
      res: LoggedValue,
      ok: Boolean
  ): F[Nothing, Unit]

  def prepare[Alg[_[_, _]]](implicit Alg: ClassTag[Alg[Any]]) = new PreparedImpl[Alg](Alg.runtimeClass)

  protected class MethodImpl[U[f[_, _]], Err: Loggable, Res: Loggable](
      cls: Class[_],
      method: String,
      args: Buffer[(String, LoggedValue)]
  ) extends BiMethod[U, Err, Res, LoggingBiMid] {
    def arg[A: Loggable](name: String, a: A) = {
      args += (name -> a)
      this
    }

    def result: LoggingBiMid[Err, Res] = new LoggingBiMid[Err, Res] {
      private[this] val argSeq = args.toSeq

      def around[F[+_, +_]: Bind: Logging.Safe](fa: F[Err, Res]): F[Err, Res] =
        onEnter(cls, method, argSeq) *>
          fa.tapBoth(
            err => onLeave(cls, method, argSeq, err, ok = false),
            res => onLeave(cls, method, argSeq, res, ok = true)
          )
    }
  }

  class PreparedImpl[U[f[_, _]]](cls: Class[_]) extends BiPrepared[U, LoggingBiMid] {
    def start[Err: Loggable, Res: Loggable](method: String) = new MethodImpl[U, Err, Res](cls, method, Buffer())
  }
}

object LoggingBiMidBuilder {
  class Default extends LoggingBiMidBuilder {
    def onEnter[F[_, _]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)])(implicit
        F: Logging.Safe[F]
    ): F[Nothing, Unit] = F.debug("entering {} {}", method, new ArgsLoggable(args))

    def onLeave[F[_, _]](
        cls: Class[_],
        method: String,
        args: Seq[(String, LoggedValue)],
        res: LoggedValue,
        ok: Boolean,
    )(implicit
        F: Logging.Safe[F]
    ): F[Nothing, Unit] =
      if (ok)
        F.debug("leaving {} {} result is {}", method, new ArgsLoggable(args), res)
      else
        F.error("error during {} {} error is {}", method, new ArgsLoggable(args), res)
  }

}
