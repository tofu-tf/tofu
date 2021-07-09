package tofu

import cats.Functor
import tofu.optics.Contains

/** Synonym for [[Context]] with explicit C as Ctx for better type inference
  *
  * There is also a nice type alias: {{{
  * import tofu.In
  *
  * val fHasMyCtx: MyCtx In F = ???
  * }}}
  */
trait WithContext[F[_], C] extends Context[F] {
  override type Ctx = C
}

/** Companion object for [[WithContext]] */
object WithContext {
  def apply[F[_], C](implicit ctx: WithContext[F, C]): WithContext[F, C] = ctx
}

final class WithContextContainsInstance[F[_], A, B](implicit wc: WithContext[F, A], lens: A Contains B)
    extends WithContext[F, B] {
  def functor: Functor[F] = wc.functor
  def context: F[B]       = wc.extract(lens).context
}
