package tofu.syntax

import cats.~>

object functionK {
  /** simple constructor of FunctionK
    * you can use it as makeFunctionK[List, Option](_.headOption)
    * credits : https://github.com/alexknvl*/
  def makeFunctionK[F[_], G[_]](maker: MakeFunctionK[F, G]): F ~> G = maker

  abstract class MakeFunctionK[F[_], G[_]] extends (F ~> G) {

    def applyArbitrary(f: F[Arbitrary]): G[Arbitrary]

    def apply[A](fa: F[A]): G[A] = applyArbitrary(fa.asInstanceOf[F[Arbitrary]]).asInstanceOf[G[A]]
  }
  type Arbitrary
}
