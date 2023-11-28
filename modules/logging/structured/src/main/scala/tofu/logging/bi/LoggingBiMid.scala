package tofu.logging.bi

import tofu.control.Bind
import tofu.higherKind.bi.BiMid
import tofu.logging.{Logging, builder}

/** Logging middleware for binary typeconstructor parameterized algebras Alg[LoggingBiMid] is a special form of implicit
  * evidence of injectable logging support generally you don't need `Logging` instance to derive this so choice of
  * logging postponed until this middleware is attached to the core instance
  */
abstract class LoggingBiMid[E, A] {
  def around[F[+_, +_]: Bind: Logging.SafeBase](fa: F[E, A]): F[E, A]

  def toMid[F[+_, +_]: Bind: Logging.SafeBase]: BiMid[F, E, A] = fx => around(fx)
}

object LoggingBiMid extends builder.LoggingBiMidBuilder.Default {
  type Of[U[_[_, _]]] = U[LoggingBiMid]
}
