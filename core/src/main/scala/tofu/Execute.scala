package tofu

import cats.effect.{Async, ContextShift}
import tofu.syntax.monadic._

import scala.concurrent.{ExecutionContext, Future}

trait Execute[F[_]] {
  def executionContext: F[ExecutionContext]

  def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A]

  def deferFuture[A](f: => Future[A]): F[A] = deferFutureAction(_ => f)
}

object Execute extends ExecuteInstance

sealed trait ExecuteInstance {
  final implicit def asyncExecute[F[_]](
      implicit ec: ExecutionContext,
      cs: ContextShift[F],
      asyncF: Async[F]
  ): Execute[F] =
    new Execute[F] {
      def async: Async[F] = asyncF

      override def executionContext: F[ExecutionContext] = ec.pure[F]

      def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A] =
        Async.fromFuture(asyncF.delay(f(ec)))
    }
}
