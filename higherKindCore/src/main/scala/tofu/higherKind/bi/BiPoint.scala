package tofu.higherKind.bi

trait BiPoint[F[_, _]] {
  def apply[E, A]: F[E, A]

  def pure[U[f[_, _]]](implicit UP: PureBK[U]): U[F] = UP.pureB(this)
}

object BiPoint {
  val unit: BiPoint[UnitBK] = new BiPoint[UnitBK] {
    override def apply[E, A]: Unit = ()
  }
}
