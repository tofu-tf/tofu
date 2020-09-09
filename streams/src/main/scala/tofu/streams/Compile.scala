package tofu.streams

/** Provides several ways of projecting streaming effect of type `F` to `G`.
  */
trait Compile[F[_], G[_]] {

  /** Compile `F` into target effect `G` by discarding all of the output values.
    */
  def drain[A](fa: F[A]): G[Unit]

  /** Compile `F` into target effect `G` by folding all of the output values into `B`.
    */
  def fold[A, B](fa: F[A])(init: B)(f: (B, A) => B): G[B]

  /** Compile `F` into target effect `G` by collecting all of the output values in a collection `C`.
    */
  def to[C[_], A](fa: F[A])(implicit ev: internal.Factory[A, C[A]]): G[C[A]]
}
