package tofu.syntax

import cats.data.Ior
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.instances.list._
import cats.instances.option._
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.option.none
import cats.syntax.ior._
import tofu.syntax.fior._

class FIorSuite extends AnyWordSpec with Matchers  {
  "TofuFIorOps#mapF" should {
    "run function well in case of right" in {
      Ior.right(1).some.mapF(i => Some(i + 1)) mustBe Some(Ior.right(2))
    }

    "run function well in case of left" in {
      Ior.left[String, Int]("").some.mapF(i => Some(i + 1)) mustBe Some(Ior.left(""))
    }

    "run function well in case of both" in {
      Ior.both("", 1).some.mapF(i => Some(i + 1)) mustBe Some(Ior.both("", 2))
    }
  }

  "TofuFIorOps#mapIn" should {
    "run function well in case of right" in {
      Ior.right(1).some.mapIn(_ + 1) mustBe Some(Ior.right(2))
    }

    "run function well in case of left" in {
      Ior.left[String, Int]("").some.mapIn(_ + 1) mustBe Some(Ior.left(""))
    }

    "run function well in case of both" in {
      Ior.both("", 1).some.mapIn(_ + 1) mustBe Some(Ior.both("", 2))
    }
  }
}
