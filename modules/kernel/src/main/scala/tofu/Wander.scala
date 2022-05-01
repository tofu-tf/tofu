package tofu

import cats.Traverse
import tofu.internal.carriers.{WanderCarrierCE2, WanderCarrierCE3}

trait Wander[F[_]] {
  def wander[T[_]: Traverse, A, B](in: T[A])(f: A => F[B]): F[T[B]]

  def wanderN[T[_]: Traverse, A, B](in: T[A], n: Int)(f: A => F[B]): F[T[B]]
}

object Wander extends WanderInstances0 {
  implicit def byCarrierCE3[F[_]](implicit carrier: WanderCarrierCE3[F]): Wander[F] = carrier.content
}

private[tofu] trait WanderInstances0 {
  implicit def byCarrierCE2[F[_]](implicit carrier: WanderCarrierCE2[F]): Wander[F] = carrier.content
}
