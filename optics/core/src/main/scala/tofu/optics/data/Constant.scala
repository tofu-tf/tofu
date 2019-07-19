package tofu.optics.data

import cats.kernel.{Monoid, Semigroup}
import cats.syntax.monoid._
import cats.{Applicative, Apply, Bifunctor, Functor}

sealed trait Constant[+A, +B] {
  def value: A
  def retag[C]: Constant[A, C]
}

object Constant extends ConstantLowPrior {
  def apply[A](a: A) = Impl(a)

  def of[B] = new Apply[B]

  class Apply[B] {
    def apply[A](a: A): Constant[A, B] = Impl(a)
  }

  final case class Impl[+A](value: A) extends Constant[A, Nothing] {
    def retag[C]: Constant[A, C] = this
  }

  implicit val bifunctor: Bifunctor[Constant] = new Bifunctor[Constant] {
    def bimap[A, B, C, D](fab: Constant[A, B])(f: A => C, g: B => D): Constant[C, D] = Impl(f(fab.value))
  }

  implicit def applicative[X: Monoid]: Applicative[Constant[X, *]] = new Applicative[Constant[X, *]] {
    def pure[A](x: A): Constant[X, A]                                         = Impl(Monoid.empty[X])
    def ap[A, B](ff: Constant[X, A => B])(fa: Constant[X, A]): Constant[X, B] = Impl(ff.value |+| fa.value)
  }
}

class ConstantLowPrior extends ConstantLowPrior2 {
  implicit def applyInstance[X: Semigroup]: Apply[Constant[X, *]] = new Apply[Constant[X, *]] {
    def ap[A, B](ff: Constant[X, A => B])(fa: Constant[X, A]): Constant[X, B] = Constant(ff.value |+| fa.value)
    def map[A, B](fa: Constant[X, A])(f: A => B): Constant[X, B]              = fa.retag
  }

}

class ConstantLowPrior2 {
  implicit def functor[X]: Functor[Constant[X, *]] = Constant.bifunctor.rightFunctor
}
