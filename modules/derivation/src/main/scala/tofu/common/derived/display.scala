package tofu.common.derived

import tofu.common.Display


/** Derivation of [[Display]] typeclass for case classes and sealed traits
 *
 * @note
 *   Derived [[Display]] instances will indent nested structures if those are supposed to be on newline. You can see
 *   examples in the tests.
 */
object display extends DisplayDerivationImpl
