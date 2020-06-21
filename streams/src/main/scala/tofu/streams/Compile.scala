package tofu.streams

trait Compile[F[_], G[_], C[_]] {
  def compile[A](fa: F[A]): G[C[A]]
}
