package tofu.streams

/** Provides several ways of projecting streaming effect of type `F` to `G`.
  */
trait Compile[F[_], G[_]] {

  def drain[A](fa: F[A]): G[Unit]

  def fold[A, B](fa: F[A])(init: B)(f: (B, A) => B): G[B]

  def to[C[_], A](fa: F[A])(implicit ev: internal.Factory[A, C[A]]): G[C[A]]
}
