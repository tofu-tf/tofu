package tofu.syntax

import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.either.catsSyntaxEitherId
import cats.instances.option._
import org.scalatest.FlatSpec
import tofu.syntax.feither._

class FEitherSuite extends FlatSpec {

  "orElseF" should "return self" in {
    assert(
      4.asRight[String].some.orElseF(None) === Some(Right(4))
    )
  }

  it should "return arg" in {
    assert(
      4.asLeft[String].some.orElseF("test".asRight[Int].some) === Some(Right("test"))
    )
  }

//    "check lazyness"
//  }
}
