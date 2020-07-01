package tofu.streams

trait CombineK[F[_]] {
  def combineK_[A](a: F[A])(b: => F[A]): F[A]
}
