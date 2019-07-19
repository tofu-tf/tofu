package tofu.control

import cats.instances.option._
import cats.syntax.coflatMap._
import cats.syntax.option._
import cats.{Applicative, Monoid}
import simulacrum._

@typeclass
trait Selective[F[_]] extends Applicative[F] {
  @noop def select[A](fo: F[Option[A]], fb: F[A]): F[A]

  def selectRight[A](fb: F[A], fo: F[Option[A]]): F[A] = select(fo, fb)

  @noop def orElses[A](fx: F[Option[A]])(fy: F[Option[A]]): F[Option[A]] = select(map(fx)(_.coflatten), fy)

  def whens[A](fb: F[Boolean])(fa: F[A]): F[Option[A]] =
    select(map(fb)(x => if (x) None else Some(none[A])), map(fa)(_.some))

  def unlesss[A](fb: F[Boolean])(fa: F[A]): F[Option[A]] = whens(map(fb)(!_))(fa)

  def optionMonoid[A]: Monoid[F[Option[A]]] = new Monoid[F[Option[A]]] {
    def empty: F[Option[A]]                                     = pure(None)
    def combine(x: F[Option[A]], y: F[Option[A]]): F[Option[A]] = orElses(x)(y)
  }

//  def takeFirst[T[_]: Foldable](xs: )
}
