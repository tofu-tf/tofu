package tofu.concurrent

import java.time.Instant
import java.{time => jt}

import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.duration._
import scala.jdk.DurationConverters._

import cats.Monad
import cats.data.ReaderT
import cats.effect.std.Dispatcher
import cats.effect.{IO, Resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import tofu.generate.GenRandom
import tofu.syntax.monadic._
import tofu.syntax.scoped._
import tofu.syntax.time._
import tofu.time.{Clock, Sleep}
import tofu.{Execute, PerformThrow, WithContext}

case class MyContext(dispatcher: Dispatcher[IO], traceId: Long)
object MyContext {
  type Reader[+A] = ReaderT[IO, MyContext, A @uncheckedVariance]
  implicit val withDispatcher: WithContext[Reader, Dispatcher[IO]] =
    WithContext[Reader, MyContext].extract(_.dispatcher)
}

class PerformSuite extends AnyFunSuite with Matchers {
  type Eff[+A] = MyContext.Reader[A]

  val p = PerformThrow[Eff]

  val genTraceId = for {
    random  <- GenRandom.instance[IO, IO]()
    traceId <- random.nextLong
  } yield traceId

  val init = for {
    dispatcher <- Dispatcher[IO]
    traceId    <- Resource.eval(genTraceId)
  } yield MyContext(dispatcher, traceId)

  def waitTime[F[_]: Clock: Sleep: Monad](start: Instant): F[jt.Duration] =
    Sleep[F].sleep(200.millis) *> now.instant[F].map(jt.Duration.between(start, _))

  def program[F[_]: PerformThrow: Monad: Execute, A](fa: F[A]) = for {
    performer <- PerformThrow[F].performer
    (fut1, _)  = performer.toFuture(fa)
    (fut2, _)  = performer.toFuture(fa)
    (fut3, _)  = performer.toFuture(fa)
    res1      <- deferFuture(_ => fut1)
    res2      <- deferFuture(_ => fut2)
    res3      <- deferFuture(_ => fut3)
  } yield List(res1, res2, res3)

  val run = for {
    start <- now.instant[Eff]
    ress  <- program(waitTime[Eff](start))
  } yield ress.map(_.toScala)

  test("parallize over performed futures") {
    import cats.effect.unsafe.implicits.global
    val ts = init.use(run.run).unsafeRunSync()
    all(ts) must (be > 200.millis and be < 400.millis)
  }
}
