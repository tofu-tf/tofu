package tofu.logging.derivation

import java.time.LocalDate
import java.util.UUID

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tofu.logging.Loggable
import tofu.logging.TethysBuilder

class DerivedLoggableSuite extends AnyFlatSpec with Matchers {

  import tofu.logging.derivation.DerivedLoggableSamples._

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

  it should "respect masking" in {
    json(
      Jak("one", 4567, 3.123456, List(1.234, 5.678))
    ) shouldBe """{"one":"...","two":"4###","three":"3.######","four":["1.###","5.###"]}"""
  }

  "MaskedBaz" should "emit Some" in {
    json(
      MaskedBaz(Some("auf"))
    ) shouldBe """{"kek":"***"}"""
  }

  it should "not emit None" in {
    json(
      MaskedBaz(None)
    ) shouldBe """{}"""
  }

  it should "not show <none> for None with ignoreOpt" in {
    Loggable[MaskedBaz].logShow(MaskedBaz(None)) shouldBe "MaskedBaz{kek=<none>}"
  }

  it should "show mask Option values in logShow" in {
    val maybeMasked = MaskedOptBaz(
      maybeStr = Some("str"),
      maybeInt = Some(123),
      maybeBool = Some(true),
      maybeDouble = Some(100.001),
      maybeStr2 = None
    )
    Loggable[MaskedOptBaz].logShow(maybeMasked) shouldBe
      "MaskedOptBaz{" +
      "maybeStr=Some(***)," +
      "maybeInt=Some(###)," +
      "maybeBool=Some(****)," +
      "maybeDouble=Some(###.###)," +
      "maybeStr2=<none>" +
      "}"
  }

  it should "show mask fields with custom masker function" in {
    val maskedCustom = MaskedCustom(
      sensitiveField = "som sensitive data",
      firstName = Some("John"),
      age = 42
    )

    json(maskedCustom) shouldBe """{"sensitiveField":"*","firstName":"J***","age":"**"}"""
    Loggable[MaskedCustom].logShow(maskedCustom) shouldBe
      "MaskedCustom{sensitiveField=*,firstName=Some(J***),age=**}"
  }

  "MaskedContra logging" should "mask fields" in {
    json(
      MaskedContra(UUID.randomUUID(), LocalDate.of(2023, 12, 1))
    ) shouldBe """{"id":"...","date":"2023-##-##"}"""
  }
}
