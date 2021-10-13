package tofu.logging
package derivation

import tofu.higherKind.derived.HigherKindedMacros
import derevo.DerivationKN3
import derevo.DerivationKN11
import tofu.logging.bi.LoggingBiMid
import derevo.PassTypeArgs
import derevo.ParamRequire
import tofu.logging.builder.LoggingMidBuilder
import tofu.logging.builder.LoggingErrMidBuilder
import tofu.logging.builder.LoggingBiMidBuilder

/** Default logging derivation mechanism for unary effect algebras
  *
  * Adds logging around the successful invocation of each method at DEBUG level.
  *
  * @note
  *   Class name is not printed by default.
  *
  * For customization create an object with the same parents and abstract type member `Result` and redefine [onEnter]
  * and [onLeave] methods of the `LoggingMidBuilder` trait.
  */
object loggingMid
    extends LoggingMidBuilder.Default with DerivationKN3[LoggingMid.Of] with PassTypeArgs with ParamRequire[Loggable] {
  type Result[A] = LoggingMid[A]
  def instance[U[f[_]]]: U[LoggingMid] = macro HigherKindedMacros.factorizeThis[U]
}

/** Default logging derivation mechanism for unary effect algebras with error reporting.
  *
  * Adds logging around the invocation of each method at DEBUG level and error alert at ERROR level
  * @note
  *   Class name is not printed by default.
  *
  * For customization create object with the same parents and abstract type member `Result` and redefine [onEnter],
  * [onLeave] and [onFault] methods of the `LoggingErrMidBuilder` trait.
  */
object loggingMidTry
    extends LoggingErrMidBuilder.DefaultImpl[Throwable] with DerivationKN3[LoggingErrMid.Try] with PassTypeArgs
    with ParamRequire[Loggable]                                                                                       {
  type Result[A] = LoggingErrMid[Throwable, A]
  def instance[U[f[_]]]: U[Result] = macro HigherKindedMacros.factorizeThis[U]
}

/** Default logging with errors derivation mechanism for binary effect algebras.
  *
  * Adds logging around invocation of each method at DEBUG level and error alert at ERROR level
  * @note
  *   Class name is not printed by default.
  *
  * For customization create object with the same parents and abstract type member `Result` and redefine [onEnter],
  * [onLeave] methods of the `LoggingBiMidBuilder` trait.
  */
object loggingBiMid
    extends LoggingBiMidBuilder.Default with DerivationKN11[LoggingBiMid.Of] with PassTypeArgs
    with ParamRequire[Loggable]                                                                                       {
  type Result[E, A] = LoggingBiMid[E, A]
  def instance[U[f[_, _]]]: U[LoggingBiMid] =
    macro HigherKindedMacros.bifactorizeThis[U]
}
