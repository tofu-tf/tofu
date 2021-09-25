package tofu.higherKind

/** Even more higher kinded function, a transformation between two higher order type-constructors
  */
trait FunctionHK[-U[F[_]], +V[F[_]]] {
  def apply[F[_]](uf: U[F]): V[F]
}

object FunctionHK {
  def make[U[f[_]], V[f[_]]] = new Applied[U, V]

  class Applied[U[f[_]], V[f[_]]](private val __ : Boolean = true) extends AnyVal           {
    type Farb[_]
    def apply(maker: Maker[U, V, Farb]): FunctionHK[U, V] = maker
  }

  abstract class Maker[U[f[_]], V[f[_]], Farb[_]]                  extends FunctionHK[U, V] {
    def apply[F[_]](uf: U[F]): V[F] = applyArb(uf.asInstanceOf[U[Farb]]).asInstanceOf[V[F]]
    def applyArb(uf: U[Farb]): V[Farb]
  }
}
