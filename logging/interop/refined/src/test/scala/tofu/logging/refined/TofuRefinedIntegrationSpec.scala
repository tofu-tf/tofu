package tofu.logging.refined

import eu.timepit.refined.types.numeric.{PosInt, PosLong}
import eu.timepit.refined.types.string.NonEmptyString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import tofu.logging.Loggable._
import tofu.logging.Loggable

class TofuRefinedIntegrationSpec extends AnyFlatSpec with Matchers {
  "derived loggable instances" should "be correct" in {
    Loggable[PosInt].logShow(PosInt.unsafeFrom(1)) mustBe "1"
    Loggable[PosLong].logShow(PosLong.unsafeFrom(1L)) mustBe "1"
    Loggable[NonEmptyString].logShow(NonEmptyString.unsafeFrom("tofu")) mustBe "tofu"
  }
}
