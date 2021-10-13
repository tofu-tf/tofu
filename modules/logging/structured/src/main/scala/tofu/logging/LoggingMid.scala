package tofu.logging

import cats.Monad
import tofu.Errors
import tofu.higherKind.Mid
import tofu.higherKind.derived.HigherKindedMacros

/** Logging middleware Alg[LoggingMid] is a special form of implicit evidence of injectable logging support generally
  * you don't need `Logging` instance to derive this so choice of logging postponed until this middleware is attached to
  * the core instance
  */
abstract class LoggingMid[A] {
  def around[F[_]: Monad: LoggingBase](fa: F[A]): F[A]

  def toMid[F[_]: Monad: LoggingBase]: Mid[F, A] = around(_)
}

object LoggingMid extends builder.LoggingMidBuilder.DefaultImpl {
  type Result[A] = LoggingMid[A]
  def instance[U[_[_]]]: U[LoggingMid] = macro HigherKindedMacros.factorizeThis[U]

  type Of[U[_[_]]] = U[LoggingMid]
}

/** Logging middleware supporting error reporting Alg[LoggingErrMid[E, *]] is a special form of implicit evidence of
  * injectable logging support generally you don't need `Logging` instance to derive this so choice of logging postponed
  * until this middleware is attached to the core instance
  */
abstract class LoggingErrMid[E, A] extends LoggingMid[A] {
  def aroundErr[F[_]: Monad: Errors[*[_], E]: LoggingBase](fa: F[A]): F[A]

  def around[F[_]: Monad: LoggingBase](fa: F[A]): F[A] = around(fa)

  def toMid[F[_]: Monad: Errors[*[_], E]: LoggingBase]: Mid[F, A] = aroundErr(_)
}

object LoggingErrMid {
  type Of[U[_[_]], E] = U[LoggingErrMid[E, *]]
  type Try[U[_[_]]]   = U[LoggingErrMid[Throwable, *]]

  object Try extends builder.LoggingErrMidBuilder.DefaultImpl[Throwable]
}
