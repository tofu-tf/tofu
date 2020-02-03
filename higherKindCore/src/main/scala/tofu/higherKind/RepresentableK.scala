package tofu.higherKind
import cats.data.Tuple2K
import cats.{FlatMap, ~>}
import simulacrum.typeclass
import tofu.syntax.functionK.funK
import tofu.syntax.monadic._

trait RepK[U[_[_]], A] {
  def apply[R[_]](ar: U[R]): R[A]
}

object RepK {
  def apply[U[_[_]]] = new Applied[U](true)

  class Applied[T[_[_]]](private val __ : Boolean) extends AnyVal {
    type Arb[_]
    def apply[A](maker: MakeRepr[T, A, Arb]): RepK[T, A] = maker
  }

  abstract class MakeRepr[T[_[_]], A, Arb[_]] extends RepK[T, A] {
    def applyArbitrary(fk: T[Arb]): Arb[A]

    def apply[F[_]](fk: T[F]): F[A] = applyArbitrary(fk.asInstanceOf[T[Arb]]).asInstanceOf[F[A]]
  }
}

@typeclass trait RepresentableK[U[_[_]]] extends MonoidalK[U] with Embed[U] {
  def tabulate[F[_]](hom: RepK[U, *] ~> F): U[F]

  override def mapK[F[_], G[_]](af: U[F])(fk: F ~> G): U[G] = tabulate(funK(repr => fk(repr(af))))

  override def productK[F[_], G[_]](af: U[F], ag: U[G]): U[Tuple2K[F, G, *]] =
    tabulate(funK(repr => Tuple2K(repr(af), repr(ag))))

  override def embed[F[_]: FlatMap](ft: F[U[F]]): U[F] = tabulate(funK(repr => ft.flatMap(repr(_))))

  override def zipWith2K[F[_], G[_], H[_]](af: U[F], ag: U[G])(f2: Function2K[F, G, H]): U[H] =
    tabulate(funK(repr => f2(repr(af), repr(ag))))

  override def pureK[F[_]](p: Point[F]): U[F] = tabulate(funK(_ => p.point))
}

object RepresentableK {

  /** simply for reference
    * continuation form of RepK makes higher order index trivial */
  def index[U[_[_]], F[_], A](tf: U[F])(repr: RepK[U, A]): F[A] = repr(tf)
}
