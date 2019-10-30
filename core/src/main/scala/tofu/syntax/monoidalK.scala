package tofu.syntax
import tofu.higherKind.{Function2K, MonoidalK, Point, PureK}

object monoidalK {
  implicit class TofuPointKOps[F[_]](private val point: Point[F]) extends AnyVal {
    def pureK[U[_[_]]](implicit U: PureK[U]): U[F] = U.pureK(point)
  }

  implicit class TofuMonoidalFOps[U[_[_]], F[_]](private val uf: U[F]) extends AnyVal {
    def zipWithK[G[_], H[_]](ug: U[G])(f2: Function2K[F, G, H])(implicit U: MonoidalK[U]): U[H] =
      U.zipWith2K(uf, ug)(f2)

    def zipWithKTo[H[_]] = new TofuMonoidalZipWithKTo[U, F, H](uf)
  }

  class TofuMonoidalZipWithKTo[U[_[_]], F[_], H[_]] private[tofu] (private val uf: U[F]) extends AnyVal {
    def apply[G[_]](ug: U[G])(f2: Function2K[F, G, H])(implicit U: MonoidalK[U]) = U.zipWith2K(uf, ug)(f2)
  }
}
