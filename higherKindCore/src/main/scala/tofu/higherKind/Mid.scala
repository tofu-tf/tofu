package tofu.higherKind
import cats.data.Chain
import cats.tagless.ApplyK
import cats.{Monoid, MonoidK, Semigroup}
import tofu.higherKind.Mid.MidCompose
import tofu.syntax.funk.funK
import tofu.syntax.monoidalK._

trait Mid[F[_], A] {
  def apply(fa: F[A]): F[A]
  @inline def attach(fa: F[A]): F[A] = apply(fa)

  def compose(that: Mid[F, A]): Mid[F, A] = that.andThen(this)
  def andThen(that: Mid[F, A]): Mid[F, A] = MidCompose(Chain(this, that))
}

object Mid extends MidInstances {
  def point[F[_]]: Point[Mid[F, *]] = new Point[Mid[F, *]] {
    override def point[A]: Mid[F, A] = x => x
  }

  /** when unification falls */
  def attach[U[f[_]]: ApplyK, F[_]](up: U[Mid[F, *]])(alg: U[F]): U[F] = up.attach(alg)

  implicit final class TofuMidAlgebraSyntax[F[_], U[f[_]]](private val self: U[Mid[F, *]]) extends AnyVal {
    def attach(alg: U[F])(implicit U: ApplyK[U]): U[F] =
      U.map2K(alg, self)(funK(t2k => t2k.second(t2k.first)))
  }

  private final case class MidCompose[F[_], A](elems: Chain[Mid[F, A]]) extends Mid[F, A] {
    override def apply(fa: F[A]): F[A] = elems.foldLeft(fa)((x, m) => m(x))
    override def compose(that: Mid[F, A]): Mid[F, A] = that match {
      case MidCompose(es) => MidCompose(elems ++ es)
      case _              => MidCompose(elems :+ that)
    }
    override def andThen(that: Mid[F, A]): Mid[F, A] = that match {
      case MidCompose(es) => MidCompose(es ++ elems)
      case _              => MidCompose(that +: elems)
    }
  }

}
trait MidInstances extends MidInstances1 {
  implicit def preMonoidK[F[_]]: MonoidK[Mid[F, *]] = new MidMonoidK[F]

  implicit def preAlgebraMonoid[F[_], U[f[_]]: MonoidalK]: Monoid[U[Mid[F, *]]] = new MidAlgebraMonoid[F, U]
}

trait MidInstances1 {
  implicit def preAlgebraMonoid[F[_], U[f[_]]: ApplyK]: Semigroup[U[Mid[F, *]]] = new MidAlgebraSemigroup[F, U]
}

class MidMonoidK[F[_]] extends MonoidK[Mid[F, *]] {
  def empty[A]: Mid[F, A]                                = fa => fa
  def combineK[A](x: Mid[F, A], y: Mid[F, A]): Mid[F, A] = fa => x(y(fa))
}

class MidAlgebraMonoid[F[_], U[f[_]]: MonoidalK] extends MidAlgebraSemigroup[F, U] with Monoid[U[Mid[F, *]]] {
  def empty: U[Mid[F, *]] = Mid.point[F].pureK[U]
}

class MidAlgebraSemigroup[F[_], U[f[_]]: ApplyK](implicit U: ApplyK[U]) extends Semigroup[U[Mid[F, *]]] {
  def combine(x: U[Mid[F, *]], y: U[Mid[F, *]]): U[Mid[F, *]] = U.map2K(x, y)(funK(t2 => fa => t2.first(t2.second(fa))))
}
