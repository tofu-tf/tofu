package tofu.streams

trait Chunks[F[_], C[_]] {

  /** Prepends a chunk onto the front of this stream.
    */
  def cons[A](fa: F[A])(c: C[A]): F[A]

  /** Returns a stream of chunks.
    */
  def chunks[A](fa: F[A]): F[C[A]]

  /** Returns a stream of chunks of size `n`.
    * The last chunk that is emitted may have less than `n` elements.
    */
  def chunkN[A](fa: F[A])(n: Int): F[C[A]]

  /** Allows to map chunks of a stream.
    */
  def mapChunks[A, B](fa: F[A])(f: C[A] => C[B]): F[B]
}
