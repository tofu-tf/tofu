package tofu

import org.scalatest.funsuite.AnyFunSuite
import cats.effect.IO

class DelaySuite extends AnyFunSuite {
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
