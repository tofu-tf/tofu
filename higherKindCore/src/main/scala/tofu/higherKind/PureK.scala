package tofu.higherKind
import simulacrum.typeclass

/**
  * Higher-order form of pointed functor
  * Having point in functor cat 1 -> F, provide point in mapped Scal () -> U[F]
  *
  * @tparam U - higher order functor
  */

@typeclass
trait PureK[U[_[_]]] {
  def pureK[F[_]](p: Point[F]): U[F]

  def unitK: U[UnitK] = pureK(Point.unit)
}
