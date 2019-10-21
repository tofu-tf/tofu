package tofu.higherKind.bi

trait FunBK[-F[_, _], +G[_, _]] {
  def apply[A, B](fab: F[A, B]): G[A, B]
}

object FunBK {
  def apply[F[_, _]] = new Applied[F]

  class Applied[F[_, _]](private val __ : Boolean = true) extends AnyVal {
    type A
    type B
    def apply[G[_, _]](maker: Maker[F, G, A, B]): FunBK[F, G] = maker
  }

  abstract class Maker[-F[_, _], +G[_, _], A1, B1] extends FunBK[F, G] {

    def applyArbitrary(fb: F[A1, B1]): G[A1, B1]

    def apply[A, B](fab: F[A, B]): G[A, B] =
      applyArbitrary(fab.asInstanceOf[F[A1, B1]]).asInstanceOf[G[A, B]]
  }
}
