package tofu.streams

/** Provides several ways of projecting streaming effect of type `F` to `G`.
  */
trait Compile[F[_], G[_]] {

  def drain[A](fa: F[A]): G[Unit]

  def fold[A, B](fa: F[A])(init: B)(f: (B, A) => B): G[B]
}

trait Consume[F[_], G[_], C[_]] extends Compile[F, G] {

  def consume[A](fa: F[A]): G[C[A]]
}
