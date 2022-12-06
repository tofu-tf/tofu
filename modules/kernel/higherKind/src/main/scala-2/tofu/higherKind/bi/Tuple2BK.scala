package tofu.higherKind.bi

final case class Tuple2BK[F[+_, +_], G[+_, +_], +A, +B](_1: F[A, B], _2: G[A, B])
