package tofu.internal
package instances

import cats.MonadError
import tofu.compat.unused
import tofu.Finally
import tofu.internal.carriers.FinallyCarrier2
import tofu.internal.carriers.FinallyCarrier3

private[tofu] trait GuaranteeInstance extends GuaranteeInstances0 {
  final implicit def interopCE3[F[_], E, Exit[_]](implicit
      @unused ev1: MonadError[F, E],
      carrier: FinallyCarrier3.Aux[F, E, Exit]
  ): Finally[F, Exit] =
    carrier.content
}

private[tofu] trait GuaranteeInstances0 {
  final implicit def interopCE2[F[_], E, Exit[_]](implicit
      @unused ev1: MonadError[F, E],
      carrier: FinallyCarrier2.Aux[F, E, Exit]
  ): Finally[F, Exit] =
    carrier.content
}
