package tofu

import cats.Traverse
import tofu.internal.EffectComp
import tofu.internal.carriers.{BoundedParallelCarrierCE2, BoundedParallelCarrierCE3}

trait BoundedParallel[F[_]] {
  def parTraverse[T[_]: Traverse, A, B](in: T[A])(f: A => F[B]): F[T[B]]

  def parTraverseN[T[_]: Traverse, A, B](in: T[A], n: Int)(f: A => F[B]): F[T[B]]
}

object BoundedParallel extends BoundedParallelInstances0 with EffectComp[BoundedParallel] {
  implicit def byCarrierCE3[F[_]](implicit carrier: BoundedParallelCarrierCE3[F]): BoundedParallel[F] = carrier.content
}

private[tofu] trait BoundedParallelInstances0 {
  implicit def byCarrierCE2[F[_]](implicit carrier: BoundedParallelCarrierCE2[F]): BoundedParallel[F] = carrier.content
}
