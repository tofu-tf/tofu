package tofu.higherKind.bi

trait UnitalBK[U[f[_, _]]] {
  def unitB: U[UnitBK]
}

object UnitalBK {
  def apply[U[f[_, _]]](implicit u: UnitalBK[U]): UnitalBK[U] = u
}

trait PureBK[U[_[_, _]]] extends UnitalBK[U] {
  def pureB[F[_, _]](point: BiPoint[F]): U[F]

  def unitB: U[UnitBK] = pureB(BiPoint.unit)
}

object PureBKK {
  def apply[U[f[_, _]]](implicit u: PureBK[U]): PureBK[U] = u
}
