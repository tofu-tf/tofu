package tofu.control

import simulacrum.typeclass

import scala.annotation.nowarn

@typeclass @nowarn("cat=unused-imports")
trait Optional[F[_]] {
  def optional[A](fa: F[A]): F[Option[A]]
}
