package tofu.higherKind
import cats.data.{Chain, Tuple2K}
import cats.tagless.{ApplyK, InvariantK}
import cats.{Monoid, MonoidK, Semigroup, ~>}
import tofu.higherKind.Mid.MidCompose
import tofu.syntax.funk.funK

trait Mid[F[_], A] {
  def apply(fa: F[A]): F[A]
  @inline def attach(fa: F[A]): F[A] = apply(fa)

  def compose(that: Mid[F, A]): Mid[F, A] = that.andThen(this)
  def andThen(that: Mid[F, A]): Mid[F, A] = MidCompose(Chain(this, that))
}

object Mid extends MidInstances {
  def point[F[_]]: Point[Mid[F, _]] = new Point[Mid[F, _]] {
    override def point[A]: Mid[F, A] = x => x
  }

  /** when unification fails */
  def attach[U[f[_]]: ApplyK, F[_]](up: U[Mid[F, _]])(alg: U[F]): U[F] = up.attach(alg)

  implicit final class TofuMidAlgebraSyntax[F[_], U[f[_]]](private val self: U[Mid[F, _]]) extends AnyVal {
    def attach(alg: U[F])(implicit U: ApplyK[U]): U[F] =
      U.map2K(alg, self)(funK[Tuple2K[F, Mid[F, _], _], F](t2k => t2k.second(t2k.first)))
  }

  private final case class MidCompose[F[_], A](elems: Chain[Mid[F, A]]) extends Mid[F, A] {
    override def apply(fa: F[A]): F[A]               = elems.foldLeft(fa)((x, m) => m(x))
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
  implicit def midMonoidK[F[_]]: MonoidK[Mid[F, _]] = new MidMonoidK[F]

  implicit def midAlgebraMonoid[F[_], U[f[_]]: MonoidalK]: Monoid[U[Mid[F, _]]] = new MidAlgebraMonoid[F, U]
}

trait MidInstances1 {
  implicit def midAlgebraSemigroup[F[_], U[f[_]]: ApplyK]: Semigroup[U[Mid[F, _]]] = new MidAlgebraSemigroup[F, U]

  private val midInvariantInstance: InvariantK[({ type L[x[_]] = Mid[x, Any] })#L] = new MidInvariantK()
  implicit def midInvariantK[A]: InvariantK[({ type L[x[_]] = Mid[x, A] })#L]      =
    midInvariantInstance.asInstanceOf[InvariantK[({ type L[x[_]] = Mid[x, A] })#L]]
}

class MidMonoidK[F[_]] extends MonoidK[Mid[F, _]] {
  def empty[A]: Mid[F, A]                                = fa => fa
  def combineK[A](x: Mid[F, A], y: Mid[F, A]): Mid[F, A] = fa => x(y(fa))
}

class MidInvariantK extends InvariantK[({ type L[x[_]] = Mid[x, Any] })#L] {
  def imapK[F[_], G[_]](af: Mid[F, Any])(fk: F ~> G)(gk: G ~> F): Mid[G, Any] = { ga =>
    fk(af(gk(ga)))
  }
}

class MidAlgebraMonoid[F[_], U[f[_]]: MonoidalK] extends MidAlgebraSemigroup[F, U] with Monoid[U[Mid[F, _]]] {
  def empty: U[Mid[F, _]] = Mid.point[F].pureK[U]
}

class MidAlgebraSemigroup[F[_], U[f[_]]](implicit U: ApplyK[U]) extends Semigroup[U[Mid[F, _]]] {
  def combine(x: U[Mid[F, _]], y: U[Mid[F, _]]): U[Mid[F, _]] =
    U.map2K(x, y)(funK[Tuple2K[Mid[F, _], Mid[F, _], _], Mid[F, _]](t2 => fa => t2.first(t2.second(fa))))
}
