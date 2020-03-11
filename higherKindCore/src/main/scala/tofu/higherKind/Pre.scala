package tofu
package higherKind
import cats.tagless.ApplyK
import cats.{Applicative, Apply, Monoid, MonoidK, Semigroup, SemigroupK, ~>}
import tofu.syntax.funk.funK
import tofu.syntax.monadic._

/**
  * newtype for [F[_], A] =>> F[Unit]
  * an algebra U[Pre[F], A] is an algebra which translates all actions to F[Unit]
  * this is useful to represent actions preceding main logic
  */
object Pre extends PreInstances {
  type T[F[_], A] <: Base with PreTag

  type Base = Any { type PreOpaque }
  trait PreTag extends Any

  def apply[A] = new PreApplier[A](true)

  class PreApplier[A](private val __ : Boolean) extends AnyVal {
    def apply[F[_]](fu: F[Unit]): T[F, A] = fu.asInstanceOf[T[F, A]]
  }

  implicit final class TofuPreSyntax[F[_], A](private val self: T[F, A]) extends AnyVal {
    def value: F[Unit] = self.asInstanceOf[F[Unit]]
  }

  def asMid[F[_]: Apply]: Pre[F, *] ~> Mid[F, *] = funK(p => fa => p.value *> fa)

  /** when unification falls */
  def attach[U[f[_]]: ApplyK, F[_]: Apply](up: U[T[F, *]])(alg: U[F]): U[F] = up.attach(alg)

  implicit final class TofuPreAlgebraSyntax[F[_], U[f[_]]](private val self: U[T[F, *]]) extends AnyVal {
    def attach(alg: U[F])(implicit U: ApplyK[U], F: Apply[F]): U[F] =
      U.map2K(self, alg)(funK(t2k => t2k.first.value *> t2k.second))
  }

  def point[F[_]](implicit F: Applicative[F]): Point[T[F, *]] = new Point[T[F, *]] {
    override def point[A]: T[F, A] = apply(F.unit)
  }
}

class PreInstances extends PreInstances1 {
  implicit def preMonoidK[F[_]: Applicative]: MonoidK[Pre[F, *]] = new PreMonoidK[F]

  implicit def preAlgebraMonoid[F[_]: Applicative, U[f[_]]: MonoidalK]: Monoid[U[Pre[F, *]]] =
    new PreAlgebraMonoid[F, U]
}

class PreInstances1 {
  implicit def preSemigroupK[F[_]: Apply]: SemigroupK[Pre[F, *]] = new PreSemigroupK[F]

  implicit def preAlgebraSemigroup[F[_]: Apply, U[f[_]]: ApplyK]: Semigroup[U[Pre[F, *]]] =
    new PreAlgebraSemigroup[F, U]
}

private class PreAlgebraSemigroup[F[_], U[f[_]]](implicit F: Apply[F], U: ApplyK[U]) extends Semigroup[U[Pre[F, *]]] {
  def combine(x: U[Pre[F, *]], y: U[Pre[F, *]]): U[Pre[F, *]] =
    U.map2K(x, y)(funK(t2k => Pre.apply(t2k.first.value *> t2k.second.value)))
}

private class PreAlgebraMonoid[F[_], U[f[_]]](implicit F: Applicative[F], U: MonoidalK[U])
    extends PreAlgebraSemigroup[F, U] with Monoid[U[Pre[F, *]]] {
  val empty: U[Pre[F, *]] = U.pureK(Pre.point)
}

private class PreSemigroupK[F[_]: Apply] extends SemigroupK[Pre[F, *]] {
  def combineK[A](x: Pre[F, A], y: Pre[F, A]): Pre[F, A] = Pre[A](x.value *> y.value)
}

private class PreMonoidK[F[_]](implicit F: Applicative[F]) extends PreSemigroupK[F] with MonoidK[Pre[F, *]] {
  def empty[A]: Pre[F, A] = Pre[A](F.unit)
}
