package tofu.logging.bi
import tofu.control.Bind
import tofu.logging.Logging
import tofu.logging.LoggedValue
import tofu.higherKind.derived.HigherKindedMacros
import scala.reflect.ClassTag
import tofu.logging.Loggable
import scala.collection.mutable.Buffer
import tofu.syntax.bind._
import tofu.logging.LogRenderer

/** logging middleware for binary tc parameterized traits */
abstract class LoggingBiMid[E, A] {
  def apply[F[+_, +_]: Bind: Logging.Safe](fa: F[E, A]): F[E, A]
}

object LoggingBiMid extends LoggingBiMidBuilder.Default {
  def instance[U[_[_, _]]]: U[LoggingBiMid] = macro HigherKindedMacros.bifactorizeThis[LoggingBiMid, U]

  type Of[U[_[_, _]]] = U[LoggingBiMid]
}

abstract class LoggingBiMidBuilder {
  def onEnter[F[+_, +_]: Logging.Safe](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)]
  ): F[Nothing, Unit]

  def onLeave[F[+_, +_]: Logging.Safe](
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

      def apply[F[+_, +_]: Bind: Logging.Safe](fa: F[Err, Res]): F[Err, Res] =
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
        F: Logging.Safe[F]
    ): F[Nothing, Unit] = F.debug("entering {}.{} {}", cls.getName(), method, new ArgsLoggable(args))

    def onLeave[F[_, _]](
        cls: Class[_],
        method: String,
        args: Seq[(String, LoggedValue)],
        res: LoggedValue,
        ok: Boolean,
    )(implicit
        F: Logging.Safe[F]
    ): F[Nothing, Unit] =
      F.debug("leaving {}.{} {}", cls.getName(), method, new ArgsLoggable(args))
  }

  class ArgsLoggable(values: Seq[(String, LoggedValue)]) extends LoggedValue {
    override def shortName: String = "arguments"

    override def toString = values.map { case (name, value) => s"$name = $value" }.mkString("(", ", ", ")")

    def logFields[I, V, @specialized R, @specialized M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = {
      values.foldLeft(r.noop(input)) { (res, p) => r.combine(res, p._2.logFields(input)) }
    }
  }
}
