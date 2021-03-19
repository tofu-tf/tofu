package tofu.logging
package derivation

import tofu.higherKind.derived.HigherKindedMacros
import derevo.DerivationKN3
import derevo.DerivationKN11
import tofu.logging.bi.LoggingBiMid
import tofu.logging.bi.LoggingBiMidBuilder
import derevo.PassTypeArgs
import derevo.ParamRequire

object loggingMid
    extends LoggingMidBuilder.Default with DerivationKN3[LoggingMid.Of] with PassTypeArgs with ParamRequire[Loggable] {
  type Result[A] = LoggingMid[A]
  def instance[U[f[_]]]: U[LoggingMid] = macro HigherKindedMacros.factorizeThis[U]
}

object loggingMidTry
    extends LoggingErrMidBuilder.DefaultImpl[Throwable] with DerivationKN3[LoggingErrMid.Try] with PassTypeArgs
    with ParamRequire[Loggable] {
  type Result[A] = LoggingErrMid[Throwable, A]
  def instance[U[f[_]]]: U[Result] = macro HigherKindedMacros.factorizeThis[U]
}

object loggingBiMid
    extends LoggingBiMidBuilder.Default with DerivationKN11[LoggingBiMid.Of] with PassTypeArgs
    with ParamRequire[Loggable] {
  type Result[E, A] = LoggingBiMid[E, A]
  def instance[U[f[_, _]]]: U[LoggingBiMid] =
    macro HigherKindedMacros.bifactorizeThis[U]
}
