package tofu.syntax

import cats.Applicative
import cats.effect.IO
import cats.effect.concurrent.Ref
import org.scalatest.flatspec.AnyFlatSpec
import tofu.{ApplicativeThrow, Errors, Handle, Raise}
import tofu.syntax.handle._
import tofu.syntax.raise._
import cats.syntax.all._
import glass.Downcast

class RetrySuite extends AnyFlatSpec {

  sealed trait Err extends Throwable

  case object Err1 extends Err
  case object Err2 extends Err

  val createCounter: IO[Ref[IO, Int]] = Ref.of[IO, Int](0)
  val times                           = 10

  def runRetry[E <: Err, F[_]: Applicative: ({ type L[x[_]] = Raise[x[_], Err] })#L: ({ type L[x[_]] = Handle[x[_], Err] })#L](
      counter: Ref[F, Int],
      times: Int
  )(implicit DC: Downcast[Err, E]): F[Unit] =
    (counter.update(_ + 1) *> Err1.raise[F, Unit])
      .retryOnly[E](times)
      .handleWith[Err1.type](_ => Applicative[F].unit)

  "runRetry[Err1, F]" should "update counter n times" in {
    (for {
      counter <- createCounter
      _       <- runRetry[Err1.type, IO](counter, times)
      state   <- counter.get
    } yield assert(state == times)).unsafeRunSync()
  }

  "runRetry[Err2, F]" should "update counter 1 time" in {
    (for {
      counter <- createCounter
      _       <- runRetry[Err2.type, IO](counter, times)
      state   <- counter.get
    } yield assert(state == 1)).unsafeRunSync()
  }
}

class RetryChecks {

  def lol[F[_]: ApplicativeThrow](v: F[Unit]): F[Unit] = {
    v.retry(3)(implicitly[Errors[F, Throwable]])
    v.retry(3)

  }
}
