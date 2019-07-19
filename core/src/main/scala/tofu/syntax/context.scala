package tofu.syntax

import tofu._

object context {
  def context[F[_]](implicit ctx: Context[F]): F[ctx.Ctx] = ctx.context

  def ask[F[_]] = new AskPA[F](true)

  class AskPA[F[_]](val __ : Boolean) extends AnyVal {
    def apply[C, A](f: C => A)(implicit ctx: F HasContext C): F[A] = ctx.ask(f)
  }

  def runContext[F[_]] = new RunContextPA[F]

  class RunContextPA[F[_]] {
    def apply[A, C, G[_]](fa: F[A])(ctx: C)(implicit runCtx: RunContext.Aux[F, G, C]): G[A] =
      runCtx.runContext(fa)(ctx)
  }

  implicit class ContextFOps[F[_], A, C](val fa: F[A])(implicit loc: F HasLocal C) {
    def local(project: C => C): F[A] = loc.local(fa)(project)
  }
}
