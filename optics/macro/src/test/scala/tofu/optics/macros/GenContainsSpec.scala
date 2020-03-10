package tofu.optics.macros

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GenContainsSpec extends AnyFunSuite with Matchers {

  test("Nested GenContains") {
    val sut = GenContains[Foo](_.b.i)

    val foo = Foo(Bar(42))

    sut.update(foo, _ + 1).b.i shouldBe 43
  }

  test("Annotation-generated optics") {
    val foobar = FooBar(42, "test")

    FooBar.contains_i.update(foobar, _ + 1).i shouldBe 43
    FooBar.contains_j.update(foobar, _.capitalize).j shouldBe "Test"
  }
}
