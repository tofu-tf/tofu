package tofu

import tofu.kernel.types.{HasContext, HasContextRun, HasLocal, HasProvide}

@deprecated("Use WithContext instead", "0.10.3")
object HasContext {
  def apply[F[_], C](implicit hc: HasContext[F, C]): HasContext[F, C] = hc
}

@deprecated("Use WithLocal instead", "0.10.3")
object HasLocal {
  def apply[F[_], C](implicit hl: HasLocal[F, C]): HasLocal[F, C] = hl
}

@deprecated("Use WithProvide instead", "0.10.3")
object HasProvide {
  def apply[F[_], G[_], C](implicit hp: HasProvide[F, G, C]): HasProvide[F, G, C] = hp
}

@deprecated("Use WithRun instead", "0.10.3")
object HasContextRun {
  def apply[F[_], G[_], C](implicit hcr: HasContextRun[F, G, C]): HasContextRun[F, G, C] = hcr
}
