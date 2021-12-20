package tofu.internal

trait DataEffectComp[TC[_[_], _]] {
  @inline final def apply[F[_], A](implicit instance: TC[F, A]): TC[F, A] = instance
}

trait EffectComp[TC[_[_]]] {
  @inline final def apply[F[_]](implicit instance: TC[F]): TC[F] = instance
}

trait Effect2Comp[TC[f[_], g[_]]] {
  @inline final def apply[F[_], G[_]](implicit instance: TC[F, G]): TC[F, G] = instance
}

trait Effect3Comp[TC[f[_], g[_], h[_]]] {
  @inline final def apply[F[_], G[_], H[_]](implicit instance: TC[F, G, H]): TC[F, G, H] = instance
}

trait DataComp[TC[_]] {
  @inline final def apply[A](implicit instance: TC[A]): TC[A] = instance
}
