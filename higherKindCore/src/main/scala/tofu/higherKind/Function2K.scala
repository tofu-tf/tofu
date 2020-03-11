package tofu.higherKind
import cats.data.Tuple2K
import cats.{FlatMap, ~>}
import tofu.syntax.funk.funK
import tofu.syntax.monadic._

trait Function2K[F[_], G[_], H[_]] {
  def apply[A](fa: F[A], ga: G[A]): H[A]

  def tupled: Tuple2K[F, G, *] ~> H = funK(t2k => apply(t2k.first, t2k.second))
}

object Function2K {
  private[this] val representableAny = new Function2KRepresentable[Any, Any]

  implicit def representableK[F[_], G[_]]: RepresentableK[Function2K[F, G, *[_]]] =
    representableAny.asInstanceOf[RepresentableK[Function2K[F, G, *[_]]]]

  def apply[F[_], G[_], H[_]](maker: MakeFunctionK[F, G, H]): Function2K[F, G, H] = maker

  def untupled[F[_], G[_], H[_]](fk: Tuple2K[F, G, *] ~> H): Function2K[F, G, H] = apply((f, g) => fk(Tuple2K(f, g)))

  abstract class MakeFunctionK[F[_], G[_], H[_]] extends Function2K[F, G, H] {

    def applyArbitrary(f: F[Arbitrary], g: G[Arbitrary]): H[Arbitrary]

    def apply[A](fa: F[A], ga: G[A]): H[A] =
      applyArbitrary(fa.asInstanceOf[F[Arbitrary]], ga.asInstanceOf[G[Arbitrary]]).asInstanceOf[H[A]]
  }
  type Arbitrary

  private class Function2KRepresentable[X[_], Y[_]] extends RepresentableK[Function2K[X, Y, *[_]]] {
    final def tabulate[F[_]](hom: RepK[Function2K[X, Y, *[_]], *] ~> F): Function2K[X, Y, F] =
      Function2K((xa, ya) => hom(RepK[Function2K[X, Y, *[_]]](_.apply(xa, ya))))

    //just optimized allocationwise redefinitions
    final override def mapK[F[_], G[_]](af: Function2K[X, Y, F])(fk: F ~> G): Function2K[X, Y, G] =
      Function2K((xa, ya) => fk(af(xa, ya)))

    final override def productK[F[_], G[_]](
        af: Function2K[X, Y, F],
        ag: Function2K[X, Y, G]
    ): Function2K[X, Y, Tuple2K[F, G, *]] =
      Function2K((xa, ya) => Tuple2K(af(xa, ya), ag(xa, ya)))

    final override def embed[F[_]: FlatMap](ft: F[Function2K[X, Y, F]]): Function2K[X, Y, F] =
      Function2K((xa, ya) => ft.flatMap(_(xa, ya)))

    override def zipWith2K[F[_], G[_], H[_]](af: Function2K[X, Y, F], ag: Function2K[X, Y, G])(
        f2: Function2K[F, G, H]
    ): Function2K[X, Y, H] =
      Function2K((xa, ya) => f2(af(xa, ya), ag(xa, ya)))

    override def pureK[F[_]](p: Point[F]): Function2K[X, Y, F] =
      Function2K((_, _) => p.point)
  }

}
