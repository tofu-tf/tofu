package tofu.higherKind

import tofu.internal.EffectCompHK

/** Higher-order form of lax unital functor Transform identity in monoidal (with Tuple2K) category of endofunctors into
  * its image
  *
  * @tparam U
  *   - higher order functor
  */
trait UnitalK[U[_[_]]] {
  def unitK: U[UnitK]
}

object UnitalK extends EffectCompHK[UnitalK]

/** Higher-order form of pointed functor Having point in functor cat 1 -> F, provide point in mapped Scal () -> U[F]
  *
  * @tparam U
  *   - higher order functor
  */
trait PureK[U[_[_]]] extends UnitalK[U] {
  def pureK[F[_]](p: Point[F]): U[F]

  def unitK: U[UnitK] = pureK(Point.unit)
}

object PureK extends EffectCompHK[PureK]
