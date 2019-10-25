package tofu.higherKind
import cats.data.Tuple2K
import cats.~>
import tofu.syntax.functionK.funK

trait Function2K[F[_], G[_], H[_]] {
  def apply[A](fa: F[A], ga: G[A]): H[A]

  def tupled: Tuple2K[F, G, *] ~> H = funK(t2k => apply(t2k.first, t2k.second))
}

object Function2K {
  def apply[F[_], G[_], H[_]](maker: MakeFunctionK[F, G, H]): Function2K[F, G, H] = maker

  def untupled[F[_], G[_], H[_]](fk: Tuple2K[F, G, *] ~> H): Function2K[F, G, H] = apply((f, g) => fk(Tuple2K(f, g)))

  abstract class MakeFunctionK[F[_], G[_], H[_]] extends Function2K[F, G, H] {

    def applyArbitrary(f: F[Arbitrary], g: G[Arbitrary]): H[Arbitrary]

    def apply[A](fa: F[A], ga: G[A]): H[A] =
      applyArbitrary(fa.asInstanceOf[F[Arbitrary]], ga.asInstanceOf[G[Arbitrary]]).asInstanceOf[H[A]]
  }
  type Arbitrary
}
