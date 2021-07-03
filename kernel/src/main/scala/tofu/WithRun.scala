package tofu

import tofu.lift.Unlift

/** Synonym for both [[RunContext]] and [[Unlift]] with explicit `C` as `Ctx` and `G` as `Lower` for better type inference
  *
  * Can be seen as transformation `F[*] = C => G[*]`
  */
trait WithRun[F[_], G[_], C] extends WithProvide[F, G, C] with WithLocal[F, C] with RunContext[F] with Unlift[G, F] {
  override type Ctx = C

  override def self = this
}

/** Companion object for [[WithRun]] */
object WithRun {
  def apply[F[_], G[_], C](implicit ctx: WithRun[F, G, C]): WithRun[F, G, C] = ctx
}