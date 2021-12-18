package tofu.logging
package builder

import scala.collection.mutable
import scala.reflect.ClassTag

import tofu.syntax.monadic._
import cats.Monad
import tofu.Errors
import tofu.syntax.handle._
import impl.ArgsLoggable

trait Builder[+T[_]] {
  def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]): Prepared[Alg, T]
}

trait Method[U[f[_]], Res, +T[_]] {
  def arg[A: Loggable](name: String, a: A): Method[U, Res, T]
  def result: T[Res]
}

trait Prepared[U[f[_]], +T[_]] {
  def start[Res: Loggable](method: String): Method[U, Res, T]
}

/** Logging middleware generator */
trait LoggingMidBuilder extends Builder[LoggingMid] {

  /** do some logging upon enter to method invocation */
  def onEnter[F[_]: Logging](cls: Class[_], method: String, args: Seq[(String, LoggedValue)]): F[Unit]

  /** do some logging after leaving method invocation with known result */
  def onLeave[F[_]: Logging](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)],
      res: LoggedValue
  ): F[Unit]

  def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]) = new PreparedImpl[Alg](Alg.runtimeClass)

  protected class MethodImpl[U[f[_]], Res: Loggable](
      cls: Class[_],
      method: String,
      args: mutable.Buffer[(String, LoggedValue)]
  ) extends Method[U, Res, LoggingMid] {
    def arg[A: Loggable](name: String, a: A): this.type = {
      args += (name -> a)
      this
    }
    def result: LoggingMid[Res]                         = new LoggingMid[Res] {
      private[this] val argSeq                                 = args.toSeq
      def around[F[_]: Monad: Logging](fa: F[Res]): F[Res] =
        onEnter(cls, method, argSeq) *> fa.flatTap(res => onLeave(cls, method, argSeq, res))
    }
  }

  protected class PreparedImpl[U[f[_]]](cls: Class[_]) extends Prepared[U, LoggingMid] {
    def start[Res: Loggable](method: String): MethodImpl[U, Res] = new MethodImpl(cls, method, mutable.Buffer())
  }
}

object LoggingMidBuilder {
  trait Default extends LoggingMidBuilder {
    def onEnter[F[_]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)])(implicit
        F: Logging[F]
    ): F[Unit] = F.debug("entering {} {}", method, new ArgsLoggable(args))

    def onLeave[F[_]](cls: Class[_], method: String, args: Seq[(String, LoggedValue)], res: LoggedValue)(implicit
        F: Logging[F]
    ): F[Unit] = F.debug("leaving {} {} with result {}", method, new ArgsLoggable(args), res)
  }

  class DefaultImpl extends Default
}

trait LoggingErrMidBuilder[E] extends LoggingMidBuilder with Builder[LoggingErrMid[E, *]] {
  def onFault[F[_]: Logging](
      cls: Class[_],
      method: String,
      args: Seq[(String, LoggedValue)],
      err: E
  ): F[Unit]

  override def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]) = new PreparedErr[Alg](Alg.runtimeClass)

  protected class MethodErrImpl[U[f[_]], Res: Loggable](
      cls: Class[_],
      method: String,
      args: mutable.Buffer[(String, LoggedValue)]
  ) extends MethodImpl[U, Res](cls, method, args) with Method[U, Res, LoggingErrMid[E, *]] {
    override def result: LoggingErrMid[E, Res] = new LoggingErrMid[E, Res] {
      private[this] val argSeq                                                     = args.toSeq
      def aroundErr[F[_]: Monad: Errors[*[_], E]: Logging](fa: F[Res]): F[Res] =
        onEnter(cls, method, argSeq) *>
          fa.onError(onFault(cls, method, argSeq, _: E))
            .flatTap(res => onLeave(cls, method, argSeq, res))
    }
  }

  class PreparedErr[U[f[_]]](cls: Class[_]) extends super.PreparedImpl[U](cls) with Prepared[U, LoggingErrMid[E, *]] {
    override def start[Res: Loggable](method: String): MethodErrImpl[U, Res] =
      new MethodErrImpl(cls, method, mutable.Buffer())
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
    )(implicit F: Logging[F]): F[Unit] =
      F.error("error during {} {} error is {}", method, new ArgsLoggable(args), err)

  }
  class DefaultImpl[E](implicit val errLoggable: Loggable[E]) extends Default[E]
}
