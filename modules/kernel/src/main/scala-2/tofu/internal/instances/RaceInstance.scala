package tofu.internal.instances

import cats.MonadError
import tofu.Race
import tofu.compat.unused
import tofu.internal.carriers.{FibersCarrier2, FibersCarrier3}

private[tofu] trait RaceInstance extends RaceInstances0 {
  final implicit def byCarrierCE3[F[_], E](implicit
      @unused FE: MonadError[F, E],
      carrier: FibersCarrier3[F, E]
  ): Race[F] = carrier.content
}

private[tofu] trait RaceInstances0 {
  final implicit def byCarrierCE2[F[_]](implicit
      carrier: FibersCarrier2[F]
  ): Race[F] = carrier.content
}
