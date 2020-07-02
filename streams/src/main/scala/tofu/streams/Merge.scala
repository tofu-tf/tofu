package tofu.streams

trait Merge[F[_]] {

  def merge[A](fa: F[A])(that: F[A]): F[A]
}
