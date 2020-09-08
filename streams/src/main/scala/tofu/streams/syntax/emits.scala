package tofu.streams.syntax

import cats.Foldable
import tofu.streams.Emits

object emits {

  def emits[F[_]]: EmitsPA[F] = new EmitsPA[F](true)

  private[syntax] final class EmitsPA[F[_]](private val __ : Boolean) extends AnyVal {
    def apply[C[_], A](as: C[A])(implicit C: Foldable[C], emits: Emits[F]): F[A] = emits.emits(as)
  }
}
