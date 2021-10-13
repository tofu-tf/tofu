package tofu.syntax

import cats.{FlatMap, ~>}
import tofu._

object context {
  def context[F[_]](implicit ctx: Context[F]): F[ctx.Ctx] = ctx.context

  def hasContext[F[_], C](implicit ctx: F WithContext C): F[C] = ctx.context

  def ask[F[_]] = new AskPA[F](true)

  private[syntax] final class AskPA[F[_]](val __ : Boolean) extends AnyVal {
    def apply[C, A](f: C => A)(implicit ctx: F WithContext C): F[A] = ctx.ask(f)
  }

  def askF[F[_]] = new AskFPA[F](true)

  private[syntax] final class AskFPA[F[_]](val __ : Boolean) extends AnyVal {
    def apply[C, A](f: C => F[A])(implicit ctx: F WithContext C, M: FlatMap[F]): F[A] = ctx.askF(f)
  }

  def runContext[F[_]] = new RunContextPA[F](true)

  private[syntax] final class RunContextPA[F[_]](val __ : Boolean) extends AnyVal {
    def apply[A, C, G[_]](fa: F[A])(ctx: C)(implicit HP: WithProvide[F, G, C]): G[A] =
      HP.runContext(fa)(ctx)
  }

  def runContextK[F[_]] = new RunContextKPA[F](true)

  private[syntax] final class RunContextKPA[F[_]](val __ : Boolean) extends AnyVal {
    def apply[C, G[_]](ctx: C)(implicit HP: WithProvide[F, G, C]): F ~> G =
      HP.runContextK(ctx)
  }

  implicit final class LocalOps[F[_], A, C](private val fa: F[A])(implicit loc: F WithLocal C) {
    def local(project: C => C): F[A] = loc.local(fa)(project)
  }

  implicit final class AltLocalOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def local_[C](project: C => C)(implicit loc: F WithLocal C): F[A] = loc.local(fa)(project)
  }
}
