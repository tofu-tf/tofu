package tofu

import org.scalatest.funsuite.AnyFunSuite
import cats.effect.IO
import cats.effect.unsafe.IORuntime

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
}
