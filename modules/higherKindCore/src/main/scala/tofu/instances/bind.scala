package tofu.instances

import cats.Monad
import tofu.control.Bind

object bind {
  implicit def monadFromBind[F[+_, +_], E](implicit bind: Bind[F]): Monad[F[E, *]] = bind.monad
}
