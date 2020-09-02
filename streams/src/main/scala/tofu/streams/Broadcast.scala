package tofu.streams

trait Broadcast[F[_]] {

  /** Broadcast each element of `F` to `processors` concurrently.
    */
  def broadcast[A](fa: F[A])(processors: F[A] => F[Unit]*): F[Unit]

  /** Broadcast each element of `F` through `processors` concurrently.
    */
  def broadcastThrough[A, B](fa: F[A])(processors: F[A] => F[B]*): F[B]
}
