package tofu
package logging

import tofu.Context

/** proof that F has contextual value and it is loggable */
trait LoggableContext[F[_]] {
  type Ctx
  implicit def loggable: Loggable[Ctx]
  implicit def context: F HasContext Ctx
}

object LoggableContext {
  def of[F[_]](implicit ctx: Context[F]) = new LoggableContextPA[F, ctx.Ctx](ctx)
  class LoggableContextPA[F[_], C](private val ctx: F HasContext C) extends AnyVal{
    def instance(implicit ctxLog: Loggable[C]): LoggableContext[F] = new LoggableContext[F] {
      type Ctx = C
      val loggable: Loggable[C]   = ctxLog
      val context: F HasContext C = ctx
    }
  }
}
