package tofu

import cats.effect.Async
import tofu.syntax.monadic._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait Execute[F[_]] {
  implicit def async: Async[F]

  def executionContext: F[ExecutionContext]

  def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A] =
    executionContext.flatMap(implicit ec =>
      async.async(callback =>
        f(ec).onComplete {
          case Failure(ex) => callback(Left(ex))
          case Success(r)  => callback(Right(r))
      }))

  def deferFuture[A](f: => Future[A]): F[A] = deferFutureAction(_ => f)
}

object Execute extends ExecuteInstance {}

sealed trait ExecuteInstance {
  final implicit def asyncExecute[F[_]](implicit ec: ExecutionContext, asyncF: Async[F]): Execute[F] =
    new Execute[F] {
      def async: Async[F] = asyncF
      override def executionContext: F[ExecutionContext] = ec.pure[F]
    }
}
