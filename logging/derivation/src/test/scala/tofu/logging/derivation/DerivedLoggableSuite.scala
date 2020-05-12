package tofu.logging
package derivation

import derevo.derive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DerivedLoggableSuite extends AnyFlatSpec with Matchers {

  import DerivedLoggableSuite._

  val foo = Foo("zaz", Some(1))

  def json[A: Loggable](a: A) = TethysBuilder(a)

  "Foo logging" should "not rendered None" in {
    json(foo.copy(kek = None)) shouldBe """{"lol":"zaz"}"""
  }
  it should "not rendered Some flat" in {
    json(foo) shouldBe """{"lol":"zaz","kek":1}"""
  }

  "Bar logging" should "hide fields" in {
    json(Bar(foo1 = Some(foo))) shouldBe "{}"
  }

  it should "unembed fields" in {
    json(Bar(foo2 = Some(foo))) shouldBe """{"lol":"zaz","kek":1}"""
  }

  it should "embed fields" in {
    json(Bar(foo3 = Some(foo))) shouldBe """{"foo3":{"lol":"zaz","kek":1}}"""
  }

  "Baz logging" should "respect object collections" in {
    json(Baz(foos = List(foo, foo))) shouldBe """{"foos":[{"lol":"zaz","kek":1},{"lol":"zaz","kek":1}],"ys":[]}"""
  }

  it should "respect primitive collections" in {
    json(Baz(ys = Vector(1, 2, 3, 4))) shouldBe """{"foos":[],"ys":[1,2,3,4]}"""
  }

  it should "respect complex primitive collections" in {
    json(
      Baz(zs = Some(List(List("one", "two"), List("three"))))
    ) shouldBe """{"foos":[],"ys":[],"zs":[["one","two"],["three"]]}"""
  }

}

object DerivedLoggableSuite {
  @derive(loggable)
  final case class Foo(lol: String, kek: Option[Long])

  @derive(loggable)
  final case class Bar(
      @hidden foo1: Option[Foo] = None,
      @unembed foo2: Option[Foo] = None,
      foo3: Option[Foo] = None
  )

  @derive(loggable)
  final case class Baz(foos: List[Foo] = Nil, ys: Vector[Int] = Vector(), zs: Option[List[List[String]]] = None)
}
