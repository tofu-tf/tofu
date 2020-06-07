package tofu.streams

trait Chunks[F[_]] {

  def chunks[A](fa: F[A]): F[Seq[A]]

  def mapChunks[A, B](fa: F[A])(f: Seq[A] => Seq[B]): F[B]
}
