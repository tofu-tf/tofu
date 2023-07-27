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
  def point[F[_, _]]: BiPoint[BiMid[F, _, _]] = new BiPoint[BiMid[F, _, _]] {
    def apply[E, A]: BiMid[F, E, A] = x => x
  }

  /** when unification fails */
  def attach[U[f[_, _]]: SemigroupalBK, F[_, _]](up: U[BiMid[F, _, _]])(alg: U[F]): U[F] = up.attach(alg)

  implicit final class TofuMidAlgebraSyntax[F[_, _], U[f[_, _]]](private val self: U[BiMid[F, _, _]]) extends AnyVal {
    def attach(alg: U[F])(implicit U: SemigroupalBK[U]): U[F] =
      U.map2b(alg, self)(Fun2BK.apply[F, BiMid[F, _, _]]((a, b) => b(a)))
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
  implicit def midMonoidBK[F[_, _]]: MonoidBK[BiMid[F, _, _]] = new BiMidMonoidK[F]

  implicit def midAlgebraMonoid[F[_, _], U[f[_, _]]: MonoidalBK]: Monoid[U[BiMid[F, _, _]]] =
    new BiMidAlgebraMonoid[F, U]
}

trait BiMidInstances1 {
  implicit def midAlgebraSemigroup[F[_, _], U[f[_, _]]: SemigroupalBK]: Semigroup[U[BiMid[F, _, _]]] =
    new BiMidAlgebraSemigroup[F, U]
}

class BiMidMonoidK[F[_, _]] extends MonoidBK[BiMid[F, _, _]] {
  def emptybk[E, A]: BiMid[F, E, A]                                         = fa => fa
  def combinebk[E, A](x: BiMid[F, E, A], y: BiMid[F, E, A]): BiMid[F, E, A] = fa => x(y(fa))
}

class BiMidAlgebraMonoid[F[_, _], U[f[_, _]]: MonoidalBK]
    extends BiMidAlgebraSemigroup[F, U] with Monoid[U[BiMid[F, _, _]]] {
  def empty: U[BiMid[F, _, _]] = BiMid.point[F].pure
}
class BiMidAlgebraSemigroup[F[_, _], U[f[_, _]]](implicit U: SemigroupalBK[U]) extends Semigroup[U[BiMid[F, _, _]]] {
  def combine(x: U[BiMid[F, _, _]], y: U[BiMid[F, _, _]]): U[BiMid[F, _, _]] =
    U.map2b(x, y)(Fun2BK.apply[BiMid[F, _, _], BiMid[F, _, _]]((m1, m2) => fa => m1(m2(fa))))
}
