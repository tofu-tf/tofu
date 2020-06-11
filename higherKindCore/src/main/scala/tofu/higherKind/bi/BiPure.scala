package tofu.higherKind.bi

trait UnitalB[U[f[_, _]]] {
  def unitB: U[UnitB]
}

object UnitalB {
  def apply[U[f[_, _]]](implicit u: UnitalB[U]): UnitalB[U] = u
}

trait PureB[U[_[_, _]]] extends UnitalB[U] {
  def pureB[F[_, _]](point: BiPoint[F]): U[F]

  def unitB: U[UnitB] = pureB(BiPoint.unit)
}

object PureB {
  def apply[U[f[_, _]]](implicit u: PureB[U]): PureB[U] = u
}
