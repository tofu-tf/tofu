package tofu.streams

trait Merge[F[_]] {

  /** Interleaves the two inputs non-deterministically.
    */
  def merge[A](fa: F[A])(that: F[A]): F[A]
}
