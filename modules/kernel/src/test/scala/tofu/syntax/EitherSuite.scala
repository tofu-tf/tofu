package tofu.syntax

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tofu.syntax.either._

class EitherSuite extends AnyWordSpec with Matchers {

  //--Helpers------------------------------------

  implicit class SuiteIdOps[A](id: A) {
    def asRightS[L]: Option[Either[L, A]] = Some(Right(id))

    def asLeftS[R]: Option[Either[A, R]] = Some(Left(id))
  }

  def defaultRight: Option[Either[String, Int]] = 4.asRightS[String]

  def defaultLeft: Option[Either[Int, String]] = 4.asLeftS[String]

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
      Right(Left("foo")).assocR mustBe Left(Left("foo"))
    }

    "return valid value for Right Right" in {
      Right(Right("foo")).assocR mustBe Right("foo")
    }

    "return valid value for Left" in {
      Left("foo").assocR mustBe Left(Left("foo"))
    }
  }
}
