package tofu.internal.instances

import cats.MonadError
import tofu.Fibers
import tofu.compat.unused
import tofu.internal.carriers.{FibersCarrier2, FibersCarrier3}

private[tofu] trait FibersInstance extends FibersInstances0 {
  final implicit def byCarrierCE3[F[_], E, Ex[_], Fib[_]](implicit
      @unused FE: MonadError[F, E],
      carrier: FibersCarrier3.Aux[F, E, Ex, Fib]
  ): Fibers[F, Ex, Fib] = carrier.content
}

private[tofu] trait FibersInstances0 {
  final implicit def byCarrierCE2[F[_], Ex[_], Fib[_]](implicit
      carrier: FibersCarrier2.Aux[F, Ex, Fib]
  ): Fibers[F, Ex, Fib] = carrier.content
}
