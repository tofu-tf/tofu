package tofu.syntax

import cats.~>

object funk {

  def makeFunctionK[F[_], G[_]](maker: ContextNT[F, G]): F ~> G = toFunK(maker)

  def funK[F[_], G[_]](maker: ContextNT[F, G]) = toFunK(maker)

  def funKFrom[F[_]] = new Applied1[F](true)

  type ContextNT[F[_], G[_]] = (arb: { type _A }) ?=> F[arb._A] => G[arb._A]

  private def toFunK[F[_], G[_]](maker: ContextNT[F, G]): F ~> G =
    new ~>[F, G] {
      def apply[A](fa: F[A]): G[A] =
        given { type _A = A } = new { type _A = A }
        maker(fa)
    }

  final class Applied1[F[_]](private val __ : Boolean) extends AnyVal {
    def apply[G[_]](maker: ContextNT[F, G]): F ~> G =
      toFunK[F, G](maker)
  }

  // RepresentableK uses Maker
  abstract class Maker[F[_], G[_], Arbitrary] extends (F ~> G) {
    def applyArbitrary(f: F[Arbitrary]): G[Arbitrary]

    def apply[A](fa: F[A]): G[A] = applyArbitrary(fa.asInstanceOf[F[Arbitrary]]).asInstanceOf[G[A]]
  }
}
