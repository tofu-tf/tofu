package tofu.optics.macros

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe

class GenContainsSpec extends AnyFunSuite:
  test("Get") {
    val lens = GenContains[Bar](_.i)

    val data = Bar(42)

    val sut = lens.get(data)

    sut shouldBe 42
  }

  test("Set") {
    val lens = GenContains[Bar](_.i)

    val data = Bar(42)

    val sut = lens.update(data, _ + 1)

    sut.i shouldBe 43
  }
