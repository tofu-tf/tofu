package tofu.optics.macros

import org.scalatest.{FunSuite, Matchers}

class GenEquivalentSpec extends FunSuite with Matchers {
  test("GenEquivalent fields") {
    val sut = GenEquivalent.fields[FooBar]

    sut.upcast((42, "test")) shouldBe FooBar(42, "test")
  }
}
