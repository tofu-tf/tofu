package tofu.higherKind
import cats.data.Tuple2K
import cats.{FlatMap, ~>}
import tofu.syntax.funk.funK
import tofu.syntax.monadic._

trait Function2K[F[_], G[_], H[_]] {
  def apply[A](fa: F[A], ga: G[A]): H[A]

  def tupled: Tuple2K[F, G, _] ~> H = funK[Tuple2K[F, G, _], H](t2k => apply(t2k.first, t2k.second))
}

object Function2K {
  type HKAny[A] = Any

  private[this] val representableAny = new Function2KRepresentable[HKAny, HKAny]

  implicit def representableK[F[_], G[_]]: RepresentableK[({ type L[x[_]] = Function2K[F, G, x] })#L] =
    representableAny.asInstanceOf[RepresentableK[({ type L[x[_]] = Function2K[F, G, x] })#L]]

  def apply[F[_], G[_], H[_]](maker: MakeFunctionK[F, G, H]): Function2K[F, G, H] = maker

  def apply[F[_], G[_]] = new Applied[F, G]

  def untupled[F[_], G[_], H[_]](fk: Tuple2K[F, G, _] ~> H): Function2K[F, G, H] =
    apply[F, G, H]((f, g) => fk(Tuple2K(f, g)))

  class Applied[F[_], G[_]](private val __ : Boolean = true) extends AnyVal {
    def apply[H[_]](maker: MakeFunctionK[F, G, H]): Function2K[F, G, H] = maker
  }

  abstract class MakeFunctionK[F[_], G[_], H[_]] extends Function2K[F, G, H] {

    def applyArbitrary(f: F[Arbitrary], g: G[Arbitrary]): H[Arbitrary]

    def apply[A](fa: F[A], ga: G[A]): H[A] =
      applyArbitrary(fa.asInstanceOf[F[Arbitrary]], ga.asInstanceOf[G[Arbitrary]]).asInstanceOf[H[A]]
  }
  type Arbitrary

  private class Function2KRepresentable[X[_], Y[_]] extends RepresentableK[({ type L[x[_]] = Function2K[X, Y, x] })#L] {
    type Function2KT[x[_]] = Function2K[X, Y, x]
    final def tabulate[F[_]](hom: RepK[Function2KT, _] ~> F): Function2K[X, Y, F] =
      Function2K[X, Y, F]((xa, ya) => hom(RepK[Function2KT](_.apply(xa, ya))))

    // just optimized allocationwise redefinitions
    final override def mapK[F[_], G[_]](af: Function2K[X, Y, F])(fk: F ~> G): Function2K[X, Y, G] =
      Function2K[X, Y, G]((xa, ya) => fk(af(xa, ya)))

    final override def productK[F[_], G[_]](
        af: Function2K[X, Y, F],
        ag: Function2K[X, Y, G]
    ): Function2K[X, Y, Tuple2K[F, G, _]] =
      Function2K[X, Y, Tuple2K[F, G, _]]((xa, ya) => Tuple2K(af(xa, ya), ag(xa, ya)))

    final override def embed[F[_]: FlatMap](ft: F[Function2K[X, Y, F]]): Function2K[X, Y, F] =
      Function2K[X, Y, F]((xa, ya) => ft.flatMap(_(xa, ya)))

    override def zipWith2K[F[_], G[_], H[_]](af: Function2K[X, Y, F], ag: Function2K[X, Y, G])(
        f2: Function2K[F, G, H]
    ): Function2K[X, Y, H] =
      Function2K[X, Y, H]((xa, ya) => f2(af(xa, ya), ag(xa, ya)))

    override def pureK[F[_]](p: Point[F]): Function2K[X, Y, F] =
      Function2K[X, Y, F]((_, _) => p.point)
  }

}
