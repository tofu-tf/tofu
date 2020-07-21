package tofu.streams

/** Provides several ways of projecting effect of type `F` to `G`.
  */
trait Compile[F[_], G[_]] {

  def drain[A](fa: F[A]): G[Unit]

  def to[C[_], A](fa: F[A])(implicit ev: scala.collection.Factory[A, C[A]]): G[C[A]]

  def fold[A, B](fa: F[A])(init: B)(f: (B, A) => B): G[B]
}
