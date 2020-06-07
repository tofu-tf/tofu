package tofu.streams

trait Compile[F[_], G[_]] {
  def compile[A](fa: F[A]): G[Seq[A]]
}
