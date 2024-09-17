package tofu.syntax

import org.scalatest.funsuite.AnyFunSuite
import cats.{Id, ~>}
import tofu.syntax.funk._

class functionKSpec extends AnyFunSuite {
  test("funK infers type args") {
    def infer(x: Id ~> Option): Unit = ()

    assertCompiles("infer(funK(Some(_)))")
  }

  test("funKFrom infers type args") {
    def infer[F[_]](x: F ~> Option): Unit = ()

    assertCompiles("infer(funKFrom[Id](Some(_)))")
  }
}
