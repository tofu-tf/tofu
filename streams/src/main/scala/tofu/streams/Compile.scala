package tofu.streams

trait Compile[F[_], G[_]] {

  type C[_]

  def compile[A](fa: F[A]): G[C[A]]
}

object Compile {

  type Aux[F[_], G[_], CH[_]] = Compile[F, G] { type C[x] = CH[x] }
}
