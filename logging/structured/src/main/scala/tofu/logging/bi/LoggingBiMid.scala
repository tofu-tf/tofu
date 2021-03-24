package tofu.logging.bi
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag

import tofu.control.Bind
import tofu.higherKind.bi.BiMid
import tofu.logging.{LogRenderer, Loggable, LoggedValue, Logging}
import tofu.syntax.bindInv._

/** Logging middleware for binary typeconstructor parameterized algebras
  * Alg[LoggingBiMid] is a special form of implicit evidence of injectable logging support
  * generally you don't need `Logging` instance to derive this
  * so choice of logging postponed until this middleware is attached to the core instance
  */
abstract class LoggingBiMid[E, A] {
  def around[F[+_, +_]: Bind: Logging.SafeBase](fa: F[E, A]): F[E, A]

  def toMid[F[+_, +_]: Bind: Logging.SafeBase]: BiMid[F, E, A] = fx => around(fx)
}

object LoggingBiMid extends LoggingBiMidBuilder.Default {
  type Of[U[_[_, _]]] = U[LoggingBiMid]
}

abstract class LoggingBiMidBuilder {

  /** do some logging upon enter to method invocation */
  def onEnter[F[+_, +_]: Logging.SafeBase](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)]
  ): F[Nothing, Unit]

  /** do some logging after leaving method invocation with known result or error */
  def onLeave[F[+_, +_]: Logging.SafeBase](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)],
      res: LoggedValue,
      ok: Boolean
  ): F[Nothing, Unit]

  def prepare[Alg[_[_, _]]](implicit Alg: ClassTag[Alg[Any]]) = new Prepared[Alg](Alg.runtimeClass)

  class Method[U[f[_, _]], Err: Loggable, Res: Loggable](
      cls: Class[_],
      method: String,
      args: Buffer[(String, LoggedValue)]
  ) {
    def arg[A: Loggable](name: String, a: A) = args += (name -> a)

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

  class Prepared[U[f[_, _]]](cls: Class[_]) {
    def start[Err: Loggable, Res: Loggable](method: String) = new Method[U, Err, Res](cls, method, Buffer())
  }
}

object LoggingBiMidBuilder {
  class Default extends LoggingBiMidBuilder {
    def onEnter[F[_, _]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)])(implicit
        F: Logging.SafeBase[F]
    ): F[Nothing, Unit] = F.debug("entering {} {}", cls.getName(), method, new ArgsLoggable(args))

    def onLeave[F[_, _]](
        cls: Class[_],
        method: String,
        args: Seq[(String, LoggedValue)],
        res: LoggedValue,
        ok: Boolean,
    )(implicit
        F: Logging.SafeBase[F]
    ): F[Nothing, Unit] =
      if (ok)
        F.debug("leaving {} {} result is {}", method, new ArgsLoggable(args), res)
      else
        F.error("error during {} {} error is {}", method, new ArgsLoggable(args), res)
  }

  class ArgsLoggable(values: Seq[(String, LoggedValue)]) extends LoggedValue {
    override def shortName: String = "arguments"

    override def toString = values.map { case (name, value) => s"$name = $value" }.mkString("(", ", ", ")")

    def logFields[I, V, @specialized R, @specialized M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = {
      values.foldLeft(r.noop(input)) { (res, p) => r.combine(res, p._2.logFields(input)) }
    }
  }
}
