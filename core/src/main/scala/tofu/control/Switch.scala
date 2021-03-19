package tofu.control

import cats.Invariant
import simulacrum.typeclass
import tofu.Void

/** monoidal with respect to Either */
@typeclass
trait Switch[F[_]] extends Invariant[F] {
  def switch[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
  def nothing: F[Nothing] = imap[Void, Nothing](skip)(_.absurd)(identity)
  def skip: F[Void]
}

object Switch extends PartialInstances[Switch]
