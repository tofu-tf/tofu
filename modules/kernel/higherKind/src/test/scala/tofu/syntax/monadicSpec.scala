package tofu.syntax

import org.scalatest.funsuite.AnyFunSuite
import tofu.syntax.monadic._

class monadicSpec extends AnyFunSuite {
  trait Thing
  test("discard should properly discard values") {
    val value: Option[Int]               = Some(4)
    val nestedValue: Option[Option[Int]] = Some(Some(3))

    assertResult(value.discard[Int])(Some(()))
    assertResult(value.discard)(Some(()))
    assertDoesNotCompile("nestedValue.discard[Int]")
    assertResult(nestedValue.discard)(Some(()))
  }
}
