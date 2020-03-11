package tofu.syntax
import cats.~>

@deprecated("use tofu.syntax.functionk._", since = "0.7.2")
object functionK {

  /** simple constructor of FunctionK
    * you can use it as makeFunctionK[List, Option](_.headOption)
    * credits : https://github.com/alexknvl*/
  def makeFunctionK[F[_], G[_]](maker: MakeFunctionK[F, G]): F ~> G = maker

  def funK[F[_], G[_]](maker: MakeFunctionK[F, G]): F ~> G = maker

  abstract class MakeFunctionK[F[_], G[_]] extends (F ~> G) {

    def applyArbitrary(f: F[Arbitrary]): G[Arbitrary]

    def apply[A](fa: F[A]): G[A] = applyArbitrary(fa.asInstanceOf[F[Arbitrary]]).asInstanceOf[G[A]]
  }

  type Arbitrary
}

object funk {

  /** simple constructor of FunctionK
    * you can use it as makeFunctionK[List, Option](_.headOption)
    * credits : https://github.com/alexknvl*/
  def makeFunctionK[F[_], G[_]] = new Applied2[F, G](true)

  def funK[F[_], G[_]] = new Applied2[F, G](true)

  def funKFrom[F[_]] = new Applied1[F](true)

  final class Applied2[F[_], G[_]](private val __ : Boolean) extends AnyVal {
    type Arbitrary

    def apply(maker: Maker[F, G, Arbitrary]): F ~> G = maker
  }

  final class Applied1[F[_]](private val __ : Boolean) extends AnyVal {
    type Arbitrary

    def apply[G[_]](maker: Maker[F, G, Arbitrary]): F ~> G = maker
  }

  abstract class Maker[F[_], G[_], Arbitrary] extends (F ~> G) {

    def applyArbitrary(f: F[Arbitrary]): G[Arbitrary]

    def apply[A](fa: F[A]): G[A] = applyArbitrary(fa.asInstanceOf[F[Arbitrary]]).asInstanceOf[G[A]]
  }
}
