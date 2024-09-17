package tofu.logging.derivation

import scala.util.matching.Regex

sealed trait MaskMode
object MaskMode {

  /** Replaces each digit with "#" and each letter with "*". Other symbols are not changed */
  case object Full extends MaskMode

  /** Replaces value with "..." */
  case object Erase extends MaskMode

  /** Same as [[Full]] but for some part of value */
  case class ForLength(offset: Int, maxLength: Int = -1) extends MaskMode {
    def this(length: Int) = this(0, length)
  }
  case class Regexp(pattern: Regex)                      extends MaskMode

  /** Allows to define a custom masker function */
  case class Custom(f: String => String) extends MaskMode
}
