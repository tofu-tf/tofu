package tofu.optics.macros

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GenEquivalentSpec extends AnyFunSuite with Matchers {
  test("GenEquivalent fields") {
    val sut = GenEquivalent.fields[FooBar]

    sut.upcast((42, "test")) shouldBe FooBar(42, "test")
  }
}
