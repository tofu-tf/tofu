package tofu.syntax

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tofu.syntax.either._

class EitherSuite extends AnyWordSpec with Matchers {

  "TofuEitherAssocLOps#accocL" should {
    "return valid value for Left Left" in {
      Left(Left("foo")).assocL mustBe Left("foo")
    }

    "return valid value for Left Right" in {
      Left(Right("foo")).assocL mustBe Right(Left("foo"))
    }

    "return valid value for Right" in {
      Right("foo").assocL mustBe Right(Right("foo"))
    }
  }

  "TofuEitherAssocROps#accocL" should {
    "return valid value for Right Left" in {
      Right(Left("foo")).assocR mustBe Left(Right("foo"))
    }

    "return valid value for Right Right" in {
      Right(Right("foo")).assocR mustBe Right("foo")
    }

    "return valid value for Left" in {
      Left("foo").assocR mustBe Left(Left("foo"))
    }
  }
}
