package tofu.logging

import scala.reflect.ClassTag

import cats.tagless.FunctorK
import cats.{Functor, Monad}
import tofu.higherKind.Mid
import cats.tagless.syntax.functorK._
import tofu.syntax.functorbk._
import tofu.syntax.monadic._
import tofu.syntax.funk._
import tofu.higherKind.bi.FunctorBK
import tofu.control.Bind
import tofu.logging.bi.LoggingBiMid
import tofu.higherKind.bi.BiMid
import tofu.higherKind.bi.FunBK
import tofu.Errors

object LoggingMidFunctions {
  def in[U[_[_]]: FunctorK, I[_]: Functor, F[_]: Monad](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingMid],
  ): I[U[Mid[F, _]]] =
    logs.forService[U[F]].map(implicit logging => lmid.mapK(funK[LoggingMid, Mid[F, _]](_.toMid)))

  def named[U[_[_]]: FunctorK, I[_]: Functor, F[_]: Monad](name: String)(implicit
      logs: Logs[I, F],
      lmid: U[LoggingMid],
  ): I[U[Mid[F, _]]] = logs.byName(name).map(implicit logging => lmid.mapK(funK[LoggingMid, Mid[F, _]](_.toMid)))

  def inBi[U[_[_, _]]: FunctorBK, I[_]: Functor, F[+_, +_]: Bind](implicit
      logs: Logs.Safe[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingBiMid],
  ): I[U[BiMid[F, _, _]]] = logs.forService[U[F]].map(implicit logging => lmid.mapb(FunBK.apply[LoggingBiMid](_.toMid)))

  def namedBi[U[_[_, _]]: FunctorBK, I[_]: Functor, F[+_, +_]: Bind](name: String)(implicit
      logs: Logs.Safe[I, F],
      lmid: U[LoggingBiMid],
  ): I[U[BiMid[F, _, _]]] = logs.byName(name).map(implicit logging => lmid.mapb(FunBK.apply[LoggingBiMid](_.toMid)))

  def errIn[U[_[_]]: FunctorK, I[_]: Functor, F[+_]: Monad, E](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
  ): I[U[Mid[F, _]]] =
    logs.forService[U[F]].map(implicit logging => lmid.mapK(funK[LoggingErrMid[E, _], Mid[F, _]](_.toMid[F])))

  def namedErr[U[_[_]]: FunctorK, I[_]: Functor, F[+_]: Monad, E](name: String)(implicit
      logs: Logs[I, F],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
  ): I[U[Mid[F, _]]] =
    logs.byName(name).map(implicit logging => lmid.mapK(funK[LoggingErrMid[E, _], Mid[F, _]](_.toMid[F])))
}
