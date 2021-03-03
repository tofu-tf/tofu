package tofu.logging

import scala.collection.mutable
import scala.reflect.ClassTag

import tofu.syntax.monadic._
import tofu.higherKind.Mid
import cats.Monad
import tofu.higherKind.derived.HigherKindedMacros

object LoggingMid extends LoggingMidBuilder.Default {
  def instance[U[_[_]]]: U[LoggingMid] = macro HigherKindedMacros.factorizeThis[LoggingMid, U]
}

abstract class LoggingMid[A] {
  def apply[F[_]: Monad: Logging](fa: F[A]): F[A]

  def toMid[F[_]: Monad: Logging]: Mid[F, A] = apply(_)
}

/** Logging middleware generator */

abstract class LoggingMidBuilder {
  def onEnter[F[_]: Logging](cls: Class[_], method: String, args: Seq[(String, LoggedValue)]): F[Unit]

  def onLeave[F[_]: Logging](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)],
      res: LoggedValue
  ): F[Unit]

  def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]) = new Prepared[Alg](Alg.runtimeClass)

  class Method[U[f[_]], Res: Loggable](
      cls: Class[_],
      method: String,
      args: mutable.Buffer[(String, LoggedValue)]
  ) {
    def arg[A: Loggable](name: String, a: A) = args += (name -> a)

    def result: LoggingMid[Res] = new LoggingMid[Res] {
      private[this] val argSeq                            = args.toSeq
      def apply[F[_]: Monad: Logging](fa: F[Res]): F[Res] =
        onEnter(cls, method, argSeq) *> fa.flatTap(res => onLeave(cls, method, argSeq, res))
    }
  }

  class Prepared[U[f[_]]](cls: Class[_]) {
    def start[Res: Loggable](method: String) = new Method[U, Res](cls, method, mutable.Buffer())
  }
}

object LoggingMidBuilder {
  class Default extends LoggingMidBuilder {
    def onEnter[F[_]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)])(implicit
        F: Logging[F]
    ): F[Unit] = F.debug("entering {}.{} {}", cls.getName(), method, new ArgsLoggable(args))

    def onLeave[F[_]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)], res: LoggedValue)(implicit
        F: Logging[F]
    ): F[Unit] = F.debug("leaving {}.{} {}", cls.getName(), method, new ArgsLoggable(args))
  }

  class ArgsLoggable(values: Seq[(String, LoggedValue)]) extends LoggedValue {
    override def shortName: String = "arguments"

    override def toString = values.map { case (name, value) => s"$name = $value" }.mkString("(", ", ", ")")

    def logFields[I, V, @specialized R, @specialized M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = {
      values.foldLeft(r.noop(input)) { (res, p) => r.combine(res, p._2.logFields(input)) }
    }
  }
}
