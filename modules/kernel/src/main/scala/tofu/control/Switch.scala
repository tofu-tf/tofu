package tofu.control

import cats.Invariant
import tofu.Void

import tofu.internal.EffectComp

/** monoidal with respect to Either */
trait Switch[F[_]] extends Invariant[F] {
  def switch[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
  def nothing: F[Nothing] = imap[Void, Nothing](skip)(_.absurd)(identity)
  def skip: F[Void]
}

object Switch extends PartialInstances[Switch] with EffectComp[Switch]
