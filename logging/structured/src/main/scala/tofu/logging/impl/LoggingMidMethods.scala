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

trait LoggingMidMethods {
  def midIn[U[_[_]]: FunctorK, I[_]: Functor, F[_]: Monad](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingMid],
  ): I[U[Mid[F, *]]] = logs.forService[U[F]].map(implicit logging => lmid.mapK(funK(_.toMid)))

  def bimidIn[U[_[_, _]]: FunctorBK, I[_]: Functor, F[+_, +_]: Bind](implicit
      logs: Logs.Safe[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingBiMid],
  ): I[U[BiMid[F, *, *]]] = logs.forService[U[F]].map(implicit logging => lmid.mapb(FunBK.apply(_.toMid)))

  def errMidIn[U[_[_]]: FunctorK, I[_]: Functor, F[+_]: Monad, E](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
  ): I[U[Mid[F, *]]] = logs.forService[U[F]].map(implicit logging => lmid.mapK(funK(_.toMid[F])))
}
