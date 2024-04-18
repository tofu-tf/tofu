package tofu

import cats.data.ReaderT
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalatest.funsuite.AnyFunSuite

class DelaySuite extends AnyFunSuite {
  implicit val rt: IORuntime = IORuntime.global

  test("IO pure delay") {
    assert(Delay[IO].delay(1253).unsafeRunSync() === 1253)
  }

  test("IO effect delay") {
    var x = 154
    assert(Delay[IO].delay {
      x = -723
      1632
    }.unsafeRunSync() === 1632)
    assert(x === -723)
  }

  test("Lift delay") {
    var x = 154
    var y = 1242

    def foo[F[_]: Delay](): F[Int] = {
      Delay[ReaderT[F, String, _]].delay {
        y = 0
        42
      }.run("str")

      Delay[ReaderT[F, String, _]].delay {
        x = -723
        1632
      }.run("str")
    }

    assert(foo[IO]().unsafeRunSync() === 1632)
    assert(x === -723)
    assert(y === 1242)
  }
}
