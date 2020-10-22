package tofu.control

import cats.Contravariant
import simulacrum.typeclass

import scala.annotation.nowarn

@typeclass @nowarn("cat=unused-imports")
trait ContravariantFilter[F[_]] extends Contravariant[F] with Optional[F] {
  def contramapFilter[A, B](fa: F[A])(f: B => Option[A]): F[B]

  def contraCollect[A, B](fa: F[A])(f: PartialFunction[B, A]): F[B] =
    contramapFilter(fa)(f.lift)

  override def optional[A](fa: F[A]): F[Option[A]] = contramapFilter(fa)(identity)

  def contraFilter[A](fa: F[A])(f: A => Boolean): F[A] =
    contramapFilter(fa)(a => if (f(a)) Some(a) else None)
}
