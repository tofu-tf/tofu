package tofu.control

import simulacrum.typeclass

@typeclass
trait Optional[F[_]] {
  def optional[A](fa: F[A]): F[Option[A]]
}
