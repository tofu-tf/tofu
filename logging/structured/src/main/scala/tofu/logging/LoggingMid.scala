package tofu.logging

import scala.collection.mutable
import scala.reflect.ClassTag

import tofu.syntax.monadic._
import tofu.higherKind.Mid
import cats.Monad
import tofu.higherKind.derived.HigherKindedMacros
import tofu.Errors
import tofu.syntax.handle._

/** Logging middleware */
abstract class LoggingMid[A] {
  def around[F[_]: Monad: LoggingBase](fa: F[A]): F[A]

  def toMid[F[_]: Monad: LoggingBase]: Mid[F, A] = around(_)
}

object LoggingMid extends LoggingMidBuilder.DefaultImpl {
  type Result[A] = LoggingMid[A]
  def instance[U[_[_]]]: U[LoggingMid] = macro HigherKindedMacros.factorizeThis[U]

  type Of[U[_[_]]] = U[LoggingMid]
}

/** Logging middleware generator */

trait LoggingMidBuilder {
  def onEnter[F[_]: LoggingBase](cls: Class[_], method: String, args: Seq[(String, LoggedValue)]): F[Unit]

  def onLeave[F[_]: LoggingBase](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)],
      res: LoggedValue
  ): F[Unit]

  def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]): Prepared[Alg] = new Prepared[Alg](Alg.runtimeClass)

  class Method[U[f[_]], Res: Loggable](
      cls: Class[_],
      method: String,
      args: mutable.Buffer[(String, LoggedValue)]
  ) {
    def arg[A: Loggable](name: String, a: A): this.type = {
      args += (name -> a)
      this
    }
    def result: LoggingMid[Res] = new LoggingMid[Res] {
      private[this] val argSeq                                 = args.toSeq
      def around[F[_]: Monad: LoggingBase](fa: F[Res]): F[Res] =
        onEnter(cls, method, argSeq) *> fa.flatTap(res => onLeave(cls, method, argSeq, res))
    }
  }

  class Prepared[U[f[_]]](cls: Class[_]) {
    def start[Res: Loggable](method: String): Method[U, Res] = new Method[U, Res](cls, method, mutable.Buffer())
  }
}

object LoggingMidBuilder {
  trait Default extends LoggingMidBuilder {
    def onEnter[F[_]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)])(implicit
        F: LoggingBase[F]
    ): F[Unit] = F.debug("entering {} {}", method, new ArgsLoggable(args))

    def onLeave[F[_]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)], res: LoggedValue)(implicit
        F: LoggingBase[F]
    ): F[Unit] = F.debug("leaving {} {} with result {}", method, new ArgsLoggable(args), res)
  }

  class DefaultImpl extends Default
}

/** Logging middleware */
abstract class LoggingErrMid[E, A] extends LoggingMid[A] {
  def aroundErr[F[_]: Monad: Errors[*[_], E]: LoggingBase](fa: F[A]): F[A]

  def around[F[_]: Monad: LoggingBase](fa: F[A]): F[A] = around(fa)

  def toMid[F[_]: Monad: Errors[*[_], E]: LoggingBase]: Mid[F, A] = aroundErr(_)
}

object LoggingErrMid {
  type Of[U[_[_]], E] = U[LoggingErrMid[E, *]]
  type Try[U[_[_]]]   = U[LoggingErrMid[Throwable, *]]

  object Try extends LoggingErrMidBuilder.DefaultImpl[Throwable]
}

trait LoggingErrMidBuilder[E] extends LoggingMidBuilder {
  def onFault[F[_]: LoggingBase](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)],
      err: E
  ): F[Unit]

  override def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]) = new PreparedErr[Alg](Alg.runtimeClass)

  class MethodErr[U[f[_]], Res: Loggable](
      cls: Class[_],
      method: String,
      args: mutable.Buffer[(String, LoggedValue)]
  ) extends Method[U, Res](cls, method, args) {
    override def result: LoggingErrMid[E, Res] = new LoggingErrMid[E, Res] {
      private[this] val argSeq                                                     = args.toSeq
      def aroundErr[F[_]: Monad: Errors[*[_], E]: LoggingBase](fa: F[Res]): F[Res] =
        onEnter(cls, method, argSeq) *>
          fa.onError(onFault(cls, method, argSeq, _: E))
            .flatTap(res => onLeave(cls, method, argSeq, res))
    }
  }

  class PreparedErr[U[f[_]]](cls: Class[_]) extends super.Prepared[U](cls) {
    override def start[Res: Loggable](method: String): MethodErr[U, Res] =
      new MethodErr[U, Res](cls, method, mutable.Buffer())
  }
}

object LoggingErrMidBuilder {
  trait Default[E] extends LoggingMidBuilder.Default with LoggingErrMidBuilder[E] {
    implicit def errLoggable: Loggable[E]

    def onFault[F[_]](
        cls: Class[_],
        method: String,
        args: Seq[(String, LoggedValue)],
        err: E
    )(implicit F: LoggingBase[F]): F[Unit] =
      F.error("error during {} {} error is {}", method, new ArgsLoggable(args), err)

  }

  class DefaultImpl[E](implicit val errLoggable: Loggable[E]) extends Default[E]
}

class ArgsLoggable(values: Seq[(String, LoggedValue)]) extends LoggedValue {
  override def shortName: String = "arguments"

  override def toString = values.map { case (name, value) => s"$name = $value" }.mkString("(", ", ", ")")

  def logFields[I, V, @specialized R, @specialized M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = {
    values.foldLeft(r.noop(input)) { (res, p) => r.combine(res, r.field(p._1, input, p._2)) }
  }
}
