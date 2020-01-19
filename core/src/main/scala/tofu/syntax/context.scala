package tofu.syntax

import cats.Monad
import tofu._

object context {
  def context[F[_]](implicit ctx: Context[F]): F[ctx.Ctx] = ctx.context

  def context[F[_], C](implicit ctx: F HasContext C): F[C] = ctx.context

  def ask[F[_]] = new AskPA[F](true)

  class AskPA[F[_]](val __ : Boolean) extends AnyVal {
    def apply[C, A](f: C => A)(implicit ctx: F HasContext C): F[A] = ctx.ask(f)
  }

  def askF[F[_]] = new AskFPA[F](true)

  class AskFPA[F[_]](val __ : Boolean) extends AnyVal {
    def apply[C, A](f: C => F[A])(implicit ctx: F HasContext C, M: Monad[F]): F[A] = ctx.askF(f)
  }

  def runContext[F[_]] = new RunContextPA[F]

  class RunContextPA[F[_]] {
    def apply[A, C, G[_]](fa: F[A])(ctx: C)(implicit runCtx: HasContextRun[F, G, C]): G[A] =
      runCtx.runContext(fa)(ctx)
  }

  implicit final class ContextFOps[F[_], A, C](private val fa: F[A])(implicit loc: F HasLocal C) {
    def local(project: C => C): F[A] = loc.local(fa)(project)
  }
}
