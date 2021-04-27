package tofu.higherKind.bi

trait BiPoint[F[_, _]] {
  def apply[E, A]: F[E, A]
}

object BiPoint {
  val unit: BiPoint[UnitBK] = new BiPoint[UnitBK] {
    override def apply[E, A]: Unit = ()
  }
}
