package tofu.control

import tofu.internal.EffectComp

trait Optional[F[_]] {
  def optional[A](fa: F[A]): F[Option[A]]
}

object Optional extends EffectComp[Optional]
