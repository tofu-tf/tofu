package tofu.example.doobie

import cats.FlatMap
import tofu.common.Console
import tofu.syntax.context.askF
import tofu.{WithContext, WithLocal}
import tofu.syntax.console._
import tofu.syntax.monadic._

// Simple context
final case class Ctx(traceId: String)

// Naive context-aware logging
trait Logging[F[_]] {
  def info(msg: String): F[Unit]
}

object Logging {
  def make[F[_]: FlatMap: Console: WithContext[*[_], Ctx]]: Logging[F] =
    msg => askF[F]((ctx: Ctx) => puts"[Logging][traceId=${ctx.traceId}] $msg")

  object ops {
    def info[F[_]](msg: String)(implicit F: Logging[F]): F[Unit] = F.info(msg)
  }
}

// Naive context-aware tracing
trait Tracing[F[_]] {
  def traced[A](opName: String)(fa: F[A]): F[A]
}

object Tracing {
  def make[F[_]: FlatMap: Console: WithLocal[*[_], Ctx]]: Tracing[F] = new Tracing[F] {
    def traced[A](opName: String)(fa: F[A]): F[A] =
      askF[F]((ctx: Ctx) => puts"[Tracing][traceId=${ctx.traceId}] $opName" *> fa)
  }

  object ops {
    implicit class TracingOps[F[_], A](private val fa: F[A]) extends AnyVal {
      def traced(opName: String)(implicit F: Tracing[F]): F[A] = F.traced(opName)(fa)
    }
  }
}
