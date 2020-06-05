package tofu.higherKind.bi

trait BiFunction2K[F[_, _], G[_, _], H[_, _]] {
  def apply[A, B](fa: F[A, B], ga: G[A, B]): H[A, B]
}

object BiFunction2K {

  implicit class Ops[F[+_, +_], G[+_, +_], H[+_, +_]](private val f: BiFunction2K[F, G, H]) extends AnyVal {
      def tupled: BiTuple2K[F, G, *, *] BiFunK H = BiFunK.apply(t => f(t._1, t._2))
  }
}
