package tofu.higherKind

import cats.data.Tuple2K
import cats.tagless.ApplyK
import cats.{Applicative, Apply, FlatMap, Monoid, MonoidK, Semigroup, SemigroupK, ~>}
import tofu.syntax.funk.funK
import tofu.syntax.monadic.*

/** A function `[F[_], A] =>> A => F[Unit]` An algebra `U[Post[F, *]]` is an algebra which translates all actions to `A
  * \=> F[Unit]`. This is useful to represent actions succeeding main logic.
  */
trait Post[F[_], A] {
  def apply(a: A): F[Unit]
}

object Post extends PostInstances {
  def point[F[_]](implicit F: Applicative[F]): Point[Post[F, _]] = new Point[Post[F, _]] {
    override def point[A]: Post[F, A] = _ => F.unit
  }

  /** when unification fails */
  def attach[U[f[_]]: ApplyK, F[_]: FlatMap](up: U[Post[F, _]])(alg: U[F]): U[F] = up.attach(alg)

  def asMid[F[_]: FlatMap]: Post[F, _] ~> Mid[F, _] = funK[Post[F, _], Mid[F, _]](p => fa => fa.flatTap(p(_)))

  implicit final class TofuPostAlgebraSyntax[F[_], U[f[_]]](private val self: U[Post[F, _]]) extends AnyVal {
    def attach(alg: U[F])(implicit U: ApplyK[U], F: FlatMap[F]): U[F] =
      U.map2K(alg, self)(funK[Tuple2K[F, Post[F, _], _], F](t2k => t2k.first.flatTap(a => t2k.second(a))))
  }
}

class PostInstances extends PostInstances1 {
  implicit def postMonoidK[F[_]: Applicative]: MonoidK[Post[F, _]] = new PostMonoidK[F]

  implicit def postAlgebraMonoid[F[_]: Applicative, U[f[_]]: MonoidalK]: Monoid[U[Post[F, _]]] =
    new PostAlgebraMonoid[F, U]
}

class PostInstances1 {
  implicit def postSemigroupK[F[_]: Apply]: SemigroupK[Post[F, _]] = new PostSemigroupK[F]

  implicit def postAlgebraSemigroup[F[_]: Apply, U[f[_]]: ApplyK]: Semigroup[U[Post[F, _]]] =
    new PostAlgebraSemigroup[F, U]
}

private class PostAlgebraSemigroup[F[_], U[f[_]]](implicit F: Apply[F], U: ApplyK[U]) extends Semigroup[U[Post[F, _]]] {
  def combine(x: U[Post[F, _]], y: U[Post[F, _]]): U[Post[F, _]] =
    U.map2K(x, y)(funK[Tuple2K[Post[F, _], Post[F, _], _], Post[F, _]](t2k => a => t2k.first(a) *> t2k.second(a)))
}

private class PostAlgebraMonoid[F[_], U[f[_]]](implicit F: Applicative[F], U: MonoidalK[U])
    extends PostAlgebraSemigroup[F, U] with Monoid[U[Post[F, _]]] {
  val empty: U[Post[F, _]] = U.pureK(Post.point)
}

private class PostSemigroupK[F[_]: Apply] extends SemigroupK[Post[F, _]] {
  def combineK[A](x: Post[F, A], y: Post[F, A]): Post[F, A] = a => x(a) *> y(a)
}

private class PostMonoidK[F[_]](implicit F: Applicative[F]) extends PostSemigroupK[F] with MonoidK[Post[F, _]] {
  def empty[A]: Post[F, A] = _ => F.unit
}
