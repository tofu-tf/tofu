package tofu

/** Synonym for [[Local]] with explicit C as Ctx for better type inference */
trait WithLocal[F[_], C] extends Local[F] with WithContext[F, C]

/** Companion object for [[WithLocal]] */
object WithLocal {
  def apply[F[_], C](implicit ctx: WithLocal[F, C]): WithLocal[F, C] = ctx
}
