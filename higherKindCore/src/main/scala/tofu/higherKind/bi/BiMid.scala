package tofu.higherKind.bi
import cats.{Monoid, Semigroup}
import tofu.higherKind.bi.BiMid.BiMidCompose

trait BiMid[F[_, _], E, A] {
  def apply(fa: F[E, A]): F[E, A]
  @inline def attach(fa: F[E, A]): F[E, A] = apply(fa)

  def compose(that: BiMid[F, E, A]): BiMid[F, E, A] = that.andThen(this)
  def andThen(that: BiMid[F, E, A]): BiMid[F, E, A] = BiMidCompose(Vector(this, that))
}

object BiMid extends BiMidInstances {
  def point[F[_, _]]: BiPoint[BiMid[F, *, *]] = new BiPoint[BiMid[F, *, *]] {
    def apply[E, A]: BiMid[F, E, A] = x => x
  }

  /** when unification fails */
  def attach[U[f[_, _]]: SemigroupalBK, F[_, _]](up: U[BiMid[F, *, *]])(alg: U[F]): U[F] = up.attach(alg)

  implicit final class TofuMidAlgebraSyntax[F[_, _], U[f[_, _]]](private val self: U[BiMid[F, *, *]]) extends AnyVal {
    def attach(alg: U[F])(implicit U: SemigroupalBK[U]): U[F] =
      U.map2b(alg, self)(Fun2BK.apply((a, b) => b(a)))
  }

  private final case class BiMidCompose[F[_, _], E, A](elems: Vector[BiMid[F, E, A]]) extends BiMid[F, E, A] {
    override def apply(fa: F[E, A]): F[E, A]                   = elems.foldLeft(fa)((x, m) => m(x))
    override def compose(that: BiMid[F, E, A]): BiMid[F, E, A] = that match {
      case BiMidCompose(es) => BiMidCompose(elems ++ es)
      case _                => BiMidCompose(elems :+ that)
    }
    override def andThen(that: BiMid[F, E, A]): BiMid[F, E, A] = that match {
      case BiMidCompose(es) => BiMidCompose(es ++ elems)
      case _                => BiMidCompose(that +: elems)
    }
  }

}
trait BiMidInstances extends BiMidInstances1 {
  implicit def midMonoidBK[F[_, _]]: MonoidBK[BiMid[F, *, *]] = new BiMidMonoidK[F]

  implicit def midAlgebraMonoid[F[_, _], U[f[_, _]]: MonoidalBK]: Monoid[U[BiMid[F, *, *]]] =
    new BiMidAlgebraMonoid[F, U]
}

trait BiMidInstances1 {
  implicit def midAlgebraSemigroup[F[_, _], U[f[_, _]]: SemigroupalBK]: Semigroup[U[BiMid[F, *, *]]] =
    new BiMidAlgebraSemigroup[F, U]
}

class BiMidMonoidK[F[_, _]] extends MonoidBK[BiMid[F, *, *]] {
  def emptybk[E, A]: BiMid[F, E, A]                                         = fa => fa
  def combinebk[E, A](x: BiMid[F, E, A], y: BiMid[F, E, A]): BiMid[F, E, A] = fa => x(y(fa))
}

class BiMidAlgebraMonoid[F[_, _], U[f[_, _]]: MonoidalBK]
    extends BiMidAlgebraSemigroup[F, U] with Monoid[U[BiMid[F, *, *]]] {
  def empty: U[BiMid[F, *, *]] = BiMid.point[F].pure[U]
}

class BiMidAlgebraSemigroup[F[_, _], U[f[_, _]]](implicit U: SemigroupalBK[U]) extends Semigroup[U[BiMid[F, *, *]]] {
  def combine(x: U[BiMid[F, *, *]], y: U[BiMid[F, *, *]]): U[BiMid[F, *, *]] =
    U.map2b(x, y)(Fun2BK.apply((m1, m2) => fa => m1(m2(fa))))
}
