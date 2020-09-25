package tofu.optics.macros

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GenEquivalentSpec extends AnyFunSuite with Matchers {
  test("GenEquivalent fields") {
    val sut = GenEquivalent.fields[FooBar]

    sut.upcast((42, "test")) shouldBe FooBar(42, "test")
    sut.toString shouldBe "FooBar<->"
  }

  test("GenEquivalent single field") {
    val sut = GenEquivalent.fields[Bar]

    sut.upcast(42) shouldBe Bar(42)
    sut.toString shouldBe "Bar<->"
  }

  test("GenEquivalent apply") {
    val sut = GenEquivalent[Bar, Int]

    sut.upcast(42) shouldBe Bar(42)
    sut.toString shouldBe "Bar<->"
  }

  test("GenEquivalent fields unit") {
    val sut = GenEquivalent.fields[Baz]

    sut.upcast(()) shouldBe Baz()
    sut.toString shouldBe "Baz<->"
  }

  test("GenEquivalent unit") {
    val sut = GenEquivalent.unit[Baz]

    sut.upcast(()) shouldBe Baz()
    sut.toString shouldBe "Baz<->"
  }

  test("GenEquivalent object unit") {
    val sut = GenEquivalent.unit[Bar.type]

    sut.upcast(()) shouldBe Bar
    sut.toString shouldBe "Bar.type<->"
  }
}
