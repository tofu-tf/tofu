package tofu

import cats.Traverse
import tofu.internal.EffectComp
import tofu.internal.carriers.{BoundedParallelCarrierCE2, BoundedParallelCarrierCE3}
import tofu.internal.instances.BoundedParallelInstance

trait BoundedParallel[F[_]] {
  def parTraverse[T[_]: Traverse, A, B](in: T[A])(f: A => F[B]): F[T[B]]

  def parTraverseN[T[_]: Traverse, A, B](in: T[A], n: Int)(f: A => F[B]): F[T[B]]
}

object BoundedParallel extends BoundedParallelInstance with EffectComp[BoundedParallel]
