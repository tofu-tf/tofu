package tofu.logging.shapeless

import shapeless.tag.@@
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import shapeless.tag
import tofu.logging.Loggable._
import tofu.logging.Loggable
import tofu.logging.shapeless.implicits._

class TofuShapelessIntegrationSpec extends AnyFlatSpec with Matchers {
  trait SomeTag
  "derived loggable instances" should "be correct" in {
    Loggable[Int @@ SomeTag].logShow(tag[SomeTag](1)) mustBe "1"
    Loggable[Long @@ SomeTag].logShow(tag[SomeTag](1L)) mustBe "1"
    Loggable[String @@ SomeTag].logShow(tag[SomeTag]("tofu")) mustBe "tofu"
  }
}
