package tofu.optics

import cats.arrow.Profunctor
import cats.syntax.functor.*
import cats.Functor
import cats.arrow.Compose
import tofu.optics.classes.PChoice
import cats.kernel.Monoid
import cats.Applicative
import cats.syntax.monoid.*
import cats.kernel.Semigroup
import cats.Apply
import tofu.optics.data.*

object data extends DataInstancesLv0 {
  type Identity[+A]             = A
  type CoKleisli[F[+_], -A, +B] = F[A] => B
  type Proxy[+A]                = Unit
  type Tagged[-A, +B]           = Unit => B
  type Constant[+A, +B]         = A
}

class CoKleisliProfunctor[F[+_]: Functor] extends Profunctor[CoKleisli[F, *, *]] {
  def dimap[A, B, C, D](fab: CoKleisli[F, A, B])(f: C => A)(g: B => D): CoKleisli[F, C, D] =
    fc => g(fab(fc.map(f)))
}

object ProxyFunctor extends Functor[Proxy] {
  def map[A, B](fa: Unit)(f: A => B): Unit = ()
}

object TaggedProfunctor extends Compose[Tagged] with PChoice[Tagged] {

  def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(
      g: B => D
  ): Tagged[C, D] = _ => g(fab(()))
  def compose[A, B, C](f: Tagged[B, C], g: Tagged[A, B]): Tagged[A, C]     = f
  def left[A, B, C](pab: Tagged[A, B]): Tagged[Either[A, C], Either[B, C]] = _ => Left(pab(()))
  def right[A, B, C](
      pab: Tagged[A, B]
  ): Tagged[Either[C, A], Either[C, B]] = _ => Right(pab((())))
  override def optional[A, B, C](
      pab: Tagged[A, B]
  ): Tagged[Option[A], Option[B]] = _ => Some(pab(()))
}

class ConstantFunctor[C] extends Functor[Constant[C, *]] {
  def map[X, Y](fa: C)(f: X => Y): C = fa
}

class ConstantApply[C: Semigroup] extends ConstantFunctor[C] with Apply[Constant[C, *]] {
  def ap[A, B](ff: C)(fa: C): C = ff |+| fa
}

class ConstantApplicative[C: Monoid] extends ConstantApply[C] with Applicative[Constant[C, *]] {
  def pure[A](x: A): Constant[C, A] = Monoid.empty[C]
}

trait DataInstancesLv0 extends DataInstancesLv1 {
  implicit val taggedProfunctor: TaggedProfunctor.type = TaggedProfunctor

  implicit def constantApplicative[C: Monoid]: Applicative[Constant[C, *]] = new ConstantApplicative
}

trait DataInstancesLv1 extends DataInstancesLv2 {
  final implicit def constantApply[C: Semigroup]: Apply[Constant[C, *]] = new ConstantApply
}

trait DataInstancesLv2 {
  final implicit def constantFunctor[C]: Functor[Constant[C, *]] = new ConstantFunctor
}
