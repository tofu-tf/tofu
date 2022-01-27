package tofu.logging

import scala.reflect.ClassTag

import cats.tagless.FunctorK
import cats.tagless.syntax.functorK._
import cats.{Functor, Monad}
import tofu.Errors
import tofu.control.Bind
import tofu.higherKind.Mid
import tofu.higherKind.bi.{BiMid, FunBK, FunctorBK}
import tofu.logging.bi.LoggingBiMid
import tofu.syntax.functorbk._
import tofu.syntax.funk._
import tofu.syntax.monadic._

object LoggingMidFunctions {
  def in[U[_[_]]: FunctorK, I[_]: Functor, F[_]: Monad](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingMid],
  ): I[U[Mid[F, *]]] = logs.forService[U[F]].map(implicit logging => lmid.mapK(funK(_.toMid)))

  def named[U[_[_]]: FunctorK, I[_]: Functor, F[_]: Monad](name: String)(implicit
      logs: Logs[I, F],
      lmid: U[LoggingMid],
  ): I[U[Mid[F, *]]] = logs.byName(name).map(implicit logging => lmid.mapK(funK(_.toMid)))

  def inBi[U[_[_, _]]: FunctorBK, I[_]: Functor, F[+_, +_]: Bind](implicit
      logs: Logs.Safe[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingBiMid],
  ): I[U[BiMid[F, *, *]]] = logs.forService[U[F]].map(implicit logging => lmid.mapb(FunBK.apply(_.toMid)))

  def namedBi[U[_[_, _]]: FunctorBK, I[_]: Functor, F[+_, +_]: Bind](name: String)(implicit
      logs: Logs.Safe[I, F],
      lmid: U[LoggingBiMid],
  ): I[U[BiMid[F, *, *]]] = logs.byName(name).map(implicit logging => lmid.mapb(FunBK.apply(_.toMid)))

  def errIn[U[_[_]]: FunctorK, I[_]: Functor, F[+_]: Monad, E](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
  ): I[U[Mid[F, *]]] = logs.forService[U[F]].map(implicit logging => lmid.mapK(funK(_.toMid[F])))

  def namedErr[U[_[_]]: FunctorK, I[_]: Functor, F[+_]: Monad, E](name: String)(implicit
      logs: Logs[I, F],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
  ): I[U[Mid[F, *]]] = logs.byName(name).map(implicit logging => lmid.mapK(funK(_.toMid[F])))
}
