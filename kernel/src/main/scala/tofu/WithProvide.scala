package tofu

import tofu.lift.Lift

/** Synonym for [[Provide]] with explicit `C` as `Ctx` and `G` as `Lower` for better type inference
  *
  * Can be seen as transformation `F[*] = C => G[*]`
  */
trait WithProvide[F[_], G[_], C] extends Provide[F] with Lift[G, F] {
  override type Lower[A] = G[A]
  override type Ctx = C

  def self = this
}

/** Companion object for [[WithProvide]] */
object WithProvide {
  def apply[F[_], G[_], C](implicit p: WithProvide[F, G, C]): WithProvide[F, G, C] = p
}