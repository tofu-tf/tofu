package tofu.control

import cats.ContravariantMonoidal
import tofu.internal.EffectComp

trait Consume[F[_]] extends Partial[F] with ContravariantMonoidal[F] with ContravariantFilter[F] {
  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    combineK(contramap(fa)(_._1), contramap(fb)(_._2))
  override def unit: F[Unit]                                = empty[Unit]

  override def trivial[A]: F[A] = empty[A]

  override def contramapFilter[A, B](fa: F[A])(f: B => Option[A]): F[B] = contramap(optional(fa))(f)
}

object Consume extends EffectComp[Consume]
