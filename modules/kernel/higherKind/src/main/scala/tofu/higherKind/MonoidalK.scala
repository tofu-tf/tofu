package tofu.higherKind
import cats.data.Tuple2K
import cats.tagless.ApplyK
import cats.~>
import tofu.internal.EffectCompHK

/** higher order form of monoidal functor for all F[_], uf: U[F] , zipWith2K(uf, unitK)(Function2K((f, _) => f)) == uf
  * for all F[_], uf: U[F] , zipWith2K(unitK, uf)(Function2K((_, f) => f)) == uf
  * @tparam U
  *   - higher order functor
  */
trait MonoidalK[U[_[_]]] extends PureK[U] with ApplyK[U] {
  def zipWith2K[F[_], G[_], H[_]](af: U[F], ag: U[G])(f2: Function2K[F, G, H]): U[H]

  override def map2K[F[_], G[_], H[_]](af: U[F], ag: U[G])(f: Tuple2K[F, G, _] ~> H): U[H] =
    zipWith2K(af, ag)(Function2K.untupled(f))

  override def mapK[F[_], G[_]](af: U[F])(fk: F ~> G): U[G] =
    zipWith2K[F, UnitK, G](af, unitK)(Function2K[F, UnitK]((f, _) => fk(f)))

  override def productK[F[_], G[_]](af: U[F], ag: U[G]): U[Tuple2K[F, G, _]] =
    zipWith2K(af, ag)(Function2K[F, G]((f, g) => Tuple2K(f, g)))
}

object MonoidalK extends EffectCompHK[MonoidalK]
