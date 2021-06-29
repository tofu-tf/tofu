package tofu.control

import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.semigroupk._
import cats.{Functor, Invariant, MonoidK}
import tofu.Void
import tofu.internal.EffectComp

trait Partial[F[_]] extends Switch[F] with Invariant[F] with MonoidK[F] with Optional[F] {
  override def optional[A](fa: F[A]): F[Option[A]] = imap(switch(empty[Unit], fa))(_.toOption)(_.toRight(()))
  def skip: F[Void]                                = empty[Void]
  override def nothing: F[Nothing]                 = empty[Nothing]

}

object Partial extends PartialInstances[Partial] with EffectComp[Partial]

trait PartialInstances[+TC[f[_]] >: Partial[f]] {
  final implicit def byCovariant[F[_]: Functor: MonoidK]: TC[F] = new Partial[F] {
    def switch[A, B](fa: F[A], fb: F[B]): F[Either[A, B]] = fa.map(_.asLeft[B]) combineK fb.map(_.asRight[A])
    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]  = Functor[F].imap(fa)(f)(g)
    def empty[A]: F[A]                                    = MonoidK[F].empty
    def combineK[A](x: F[A], y: F[A]): F[A]               = x combineK y
  }
}
