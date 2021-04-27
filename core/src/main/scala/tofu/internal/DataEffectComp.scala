package tofu.internal

trait DataEffectComp[TC[_[_], _]] {
  def apply[F[_], A](implicit instance: TC[F, A]): TC[F, A] = instance
}
