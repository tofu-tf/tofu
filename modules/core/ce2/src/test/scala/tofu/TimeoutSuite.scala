package tofu

import cats.data.ReaderT
import cats.effect.{ContextShift, IO, Timer}
import cats.{Functor, Monad}
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

class TimeoutSuite extends AnyFunSuite {
  private val executionContext: ExecutionContext = ExecutionContext.Implicits.global
  private implicit val cs: ContextShift[IO]      = IO.contextShift(executionContext)
  private implicit val timer: Timer[IO]  = IO.timer(executionContext)

  test("Timeout should cancel") {
    val result = Timeout[IO].timeoutTo[Int](IO.never, 50.milli, IO(42))
    assert(result.unsafeRunSync() === 42)
  }

  test("Timeout with ReaderT should cancel") {
    def result[F[_]: Monad: Timeout](never: F[Nothing]): ReaderT[F, Int, Int] =
      Timeout[ReaderT[F, Int, _]]
        .timeoutTo[Int](
          ReaderT.liftF[F, Int, Int](Functor[F].widen[Nothing, Int](never)),
          50.milli,
          ReaderT.pure[F, Int, Int](42)
        )

    assert(result[IO](IO.never).run(0).unsafeRunSync() === 42)
  }

  test("Timeout with ReaderT should have context") {
    def result[F[_]: Monad: Timeout](never: F[Nothing]): ReaderT[F, Int, Int] =
      Timeout[ReaderT[F, Int, _]]
        .timeoutTo[Int](
          ReaderT.ask[F, Int],
          24.hours,
          ReaderT.liftF[F, Int, Int](Functor[F].widen[Nothing, Int](never))
        )

    assert(result[IO](IO.never).run(1242).unsafeRunSync() === 1242)
  }

  test("Timeout with ReaderT should have context in fallback") {
    def result[F[_]: Monad: Timeout](never: F[Nothing]): ReaderT[F, Int, Int] =
      Timeout[ReaderT[F, Int, _]]
        .timeoutTo[Int](
          ReaderT.liftF[F, Int, Int](Functor[F].widen[Nothing, Int](never)),
          50.milli,
          ReaderT.ask[F, Int]
        )

    assert(result[IO](IO.never).run(1242).unsafeRunSync() === 1242)
  }
}
