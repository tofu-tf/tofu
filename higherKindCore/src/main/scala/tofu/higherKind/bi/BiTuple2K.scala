package tofu.higherKind.bi

final case class BiTuple2K[F[+_, +_], G[+_, +_], +A, +B](_1: F[A, B], _2: G[A, B])
