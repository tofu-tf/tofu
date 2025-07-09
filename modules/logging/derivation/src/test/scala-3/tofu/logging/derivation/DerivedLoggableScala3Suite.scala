package tofu.logging.derivation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tofu.logging.Loggable
import tofu.logging.TethysBuilder

class DerivedLoggableScala3Suite extends AnyFlatSpec with Matchers:

  import tofu.logging.derivation.DerivedLoggableSamples.*

  def json[A: Loggable](a: A) = TethysBuilder(a)

  "SealedTraitEnum singleton enum Loggable" should "correctly log" in:
    import SealedTraitEnum.*
    json(Container(A, B, C)) shouldBe """{"a":"A","b":"B","c":"C"}"""

  "Scala3Enum singleton enum Loggable" should "correctly log" in:
    import Scala3Enum.*
    json(Container(A, B, C)) shouldBe """{"a":"A","b":"B","c":"C"}"""
