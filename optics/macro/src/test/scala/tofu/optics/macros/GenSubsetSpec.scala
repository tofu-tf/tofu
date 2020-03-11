package tofu.optics.macros

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GenSubsetSpec extends AnyFunSuite with Matchers {
  test("Subset narrow") {
    A.b.narrow(C) shouldBe Left(C)
    A.b.narrow(B) shouldBe Right(B)
  }
}
