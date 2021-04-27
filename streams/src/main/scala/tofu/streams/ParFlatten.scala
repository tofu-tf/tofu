package tofu.streams

trait ParFlatten[F[_]] {

  /** Flattens a stream of streams into a stream by executing a concurrent merge non-deterministically.
    * Up to `n` streams may be consumed in parallel.
    */
  def parFlatten[A](ffa: F[F[A]])(maxConcurrent: Int): F[A]

  /** Flattens a stream of streams into a stream by executing a concurrent merge non-deterministically.
    * Like [[parFlatten]] but does not limit the degree of concurrency.
    */
  final def parFlattenUnbounded[A](ffa: F[F[A]]): F[A] = parFlatten(ffa)(Int.MaxValue)
}

object ParFlatten {
  def apply[F[_]](implicit ev: ParFlatten[F]): ParFlatten[F] = ev
}
