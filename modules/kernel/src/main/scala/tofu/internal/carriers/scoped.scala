package tofu.internal
package carriers

import tofu.ScopedExecute

trait ScopedCarrier2[T, F[_]] extends ScopedExecute[T, F]

object ScopedCarrier2 extends ScopedCarrier2Macro
trait ScopedCarrier3[T, F[_]] extends ScopedExecute[T, F]

object ScopedCarrier3 extends ScopedCarrier3Macro
