package tofu
package higherKind
import cats.data.Tuple2K
import cats.tagless.ApplyK
import cats.{Applicative, Apply, Monoid, MonoidK, Semigroup, SemigroupK, ~>}
import tofu.syntax.funk.funK
import tofu.syntax.monadic.*

/** Newtype for `[F[_], A] =>> F[Unit]`. An algebra `U[Pre[F, _]]` is an algebra which translates all actions to
  * `F[Unit]`. This is useful to represent actions preceding main logic.
  */
object Pre extends PreInstances {
  type T[F[_], A] <: Base with PreTag

  type Unwrap[F[_], A] = F[Unit]

  def wrap[U[_[_]], F[_]](ufu: U[Unwrap[F, _]]): U[T[F, _]]    = ufu.asInstanceOf[U[T[F, _]]]
  def unwrap[U[_[_]], F[_]](upre: U[T[F, _]]): U[Unwrap[F, _]] = upre.asInstanceOf[U[Unwrap[F, _]]]

  type Base = Any { type PreOpaque }
  trait PreTag extends Any

  def apply[A] = new PreApplier[A](true)

  class PreApplier[A](private val __ : Boolean) extends AnyVal {
    def apply[F[_]](fu: F[Unit]): T[F, A] = fu.asInstanceOf[T[F, A]]
  }

  implicit final class TofuPreSyntax[F[_], A](private val self: T[F, A]) extends AnyVal {
    def value: F[Unit] = self.asInstanceOf[F[Unit]]
  }

  def asMid[F[_]: Apply]: Pre[F, _] ~> Mid[F, _] = funK[Pre[F, _], Mid[F, _]](p => fa => p.value *> fa)

  /** when unification fails */
  def attach[U[f[_]]: ApplyK, F[_]: Apply](up: U[T[F, _]])(alg: U[F]): U[F] = up.attach(alg)

  implicit final class TofuPreAlgebraSyntax[F[_], U[f[_]]](private val self: U[T[F, _]]) extends AnyVal {
    def attach(alg: U[F])(implicit U: ApplyK[U], F: Apply[F]): U[F] =
      U.map2K(self, alg)(funK[Tuple2K[T[F, _], F, _], F](t2k => t2k.first.value *> t2k.second))
  }

  def point[F[_]](implicit F: Applicative[F]): Point[T[F, _]] = new Point[T[F, _]] {
    override def point[A]: T[F, A] = apply(F.unit)
  }
}

class PreInstances extends PreInstances1 {
  implicit def preMonoidK[F[_]: Applicative]: MonoidK[Pre[F, _]] = new PreMonoidK[F]

  implicit def preAlgebraMonoid[F[_]: Applicative, U[f[_]]: MonoidalK]: Monoid[U[Pre[F, _]]] =
    new PreAlgebraMonoid[F, U]
}

class PreInstances1 {
  implicit def preSemigroupK[F[_]: Apply]: SemigroupK[Pre[F, _]] = new PreSemigroupK[F]

  implicit def preAlgebraSemigroup[F[_]: Apply, U[f[_]]: ApplyK]: Semigroup[U[Pre[F, _]]] =
    new PreAlgebraSemigroup[F, U]
}

private class PreAlgebraSemigroup[F[_], U[f[_]]](implicit F: Apply[F], U: ApplyK[U]) extends Semigroup[U[Pre[F, _]]] {
  def combine(x: U[Pre[F, _]], y: U[Pre[F, _]]): U[Pre[F, _]] =
    U.map2K(x, y)(
      funK[Tuple2K[Pre[F, _], Pre[F, _], _], Pre[F, _]](t2k => Pre.apply(t2k.first.value *> t2k.second.value))
    )
}

private class PreAlgebraMonoid[F[_], U[f[_]]](implicit F: Applicative[F], U: MonoidalK[U])
    extends PreAlgebraSemigroup[F, U] with Monoid[U[Pre[F, _]]] {
  val empty: U[Pre[F, _]] = U.pureK(Pre.point)
}

private class PreSemigroupK[F[_]: Apply] extends SemigroupK[Pre[F, _]] {
  def combineK[A](x: Pre[F, A], y: Pre[F, A]): Pre[F, A] = Pre[A](x.value *> y.value)
}

private class PreMonoidK[F[_]](implicit F: Applicative[F]) extends PreSemigroupK[F] with MonoidK[Pre[F, _]] {
  def empty[A]: Pre[F, A] = Pre[A](F.unit)
}
