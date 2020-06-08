package tofu.streams

trait Chunks[F[_]] {

  def chunks[A](fa: F[A]): F[List[A]]

  def mapChunks[A, B](fa: F[A])(f: List[A] => List[B]): F[B]
}
