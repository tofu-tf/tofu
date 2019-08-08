package tofu.optics.macros

import org.scalatest.{FunSuite, Matchers}

class GenSubsetSpec extends FunSuite with Matchers {
  test("Subset narrow") {
    A.b.narrow(C) shouldBe Left(C)
    A.b.narrow(B) shouldBe Right(B)
  }
}
