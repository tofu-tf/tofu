package tofu.internal
package carriers

import tofu.Delay

trait DelayCarrier2[F[_]] extends Delay[F]

object DelayCarrier2 extends DelayCarrier2Macro


trait DelayCarrier3[F[_]] extends Delay[F]

object DelayCarrier3 extends DelayCarrier3Macro
