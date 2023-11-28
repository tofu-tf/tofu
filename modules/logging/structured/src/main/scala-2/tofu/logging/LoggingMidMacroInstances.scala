package tofu.logging

import tofu.higherKind.derived.HigherKindedMacros

trait LoggingMidMacroInstances {
  def instance[U[_[_]]]: U[LoggingMid] = macro HigherKindedMacros.factorizeThis[U]
}
