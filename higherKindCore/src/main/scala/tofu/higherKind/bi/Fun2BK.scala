package tofu.higherKind.bi

trait Fun2BK[F[_, _], G[_, _], H[_, _]] {
  def apply[A, B](fa: F[A, B], ga: G[A, B]): H[A, B]
}

object Fun2BK {

  implicit class Ops[F[+_, +_], G[+_, +_], H[+_, +_]](private val f: Fun2BK[F, G, H]) extends AnyVal {
    def tupled: Tuple2BK[F, G, *, *] FunBK H = FunBK.apply(t => f(t._1, t._2))
  }

  def apply[F[_, _], G[_, _]] = new Applied[F, G]

  class Applied[F[_, _], G[_, _]](private val __ : Boolean = true) extends AnyVal {
    type A1
    type B1
    def apply[H[_, _]](maker: Maker[F, G, H, A1, B1]): Fun2BK[F, G, H] = maker
  }

  abstract class Maker[F[_, _], G[_, _], H[_, _], A1, B1] extends Fun2BK[F, G, H] {
    def applyArbitrary(f: F[A1, B1], g: G[A1, B1]): H[A1, B1]

    def apply[A, B](fa: F[A, B], ga: G[A, B]): H[A, B] =
      applyArbitrary(fa.asInstanceOf[F[A1, B1]], ga.asInstanceOf[G[A1, B1]]).asInstanceOf[H[A, B]]
  }
}
