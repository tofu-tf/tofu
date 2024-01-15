package tofu.internal

trait EffectCompHK[TC[mod[f[_]]]] {
  def apply[Mod[f[_]]](implicit instance: TC[Mod]): TC[Mod] = instance
}
