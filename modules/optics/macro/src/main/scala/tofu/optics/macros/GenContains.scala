package tofu.optics.macros

import tofu.optics.Contains
import tofu.optics.macros.internal.MacroImpl

class GenContains[A] {

  /** generate a [[Contains]] between a case class `S` and one of its field */
  def apply[B](field: A => B): Contains[A, B] = macro MacroImpl.genContains_impl[A, B]
}

object GenContains {
  def apply[A] = new GenContains[A]
}
