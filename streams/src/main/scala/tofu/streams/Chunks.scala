package tofu.streams

trait Chunks[F[_], C[_]] {

  def chunks[A](fa: F[A]): F[C[A]]

  def mapChunks[A, B](fa: F[A])(f: C[A] => C[B]): F[B]
}
