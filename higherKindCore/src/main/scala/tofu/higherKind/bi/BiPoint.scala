package tofu.higherKind.bi

trait BiPoint[F[_, _]] {
  def apply[E, A]: F[E, A]
}

object BiPoint {
  val unit: BiPoint[UnitB] = new BiPoint[UnitB] {
    override def apply[E, A]: Unit = ()
  }
}
