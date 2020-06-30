package tofu.higherKind.bi

trait RepBK[U[f[_, _]], E, A] {
  def apply[R[_, _]](ar: U[R]): R[E, A]
}

object RepBK {
  def apply[U[f[_, _]]] = new Applied[U]
  def mk[U[_[_, _]]]    = new Applied[U]

  class Applied[T[_[_, _]]](private val __ : Boolean = true) extends AnyVal {
    type Arb[_, _]
    def apply[E, A](maker: MakeRepr[T, E, A, Arb]): RepBK[T, E, A] = maker
  }

  abstract class MakeRepr[T[_[_, _]], E, A, Arb[_, _]] extends RepBK[T, E, A] {
    def applyArbitrary(fk: T[Arb]): Arb[E, A]

    def apply[F[_, _]](fk: T[F]): F[E, A] = applyArbitrary(fk.asInstanceOf[T[Arb]]).asInstanceOf[F[E, A]]
  }
}
