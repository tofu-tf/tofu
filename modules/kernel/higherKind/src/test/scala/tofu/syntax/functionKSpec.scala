package tofu.syntax

import org.scalatest.funsuite.AnyFunSuite
import cats.{Id, ~>}

class functionKSpec extends AnyFunSuite {
  def infer1(x: Id ~> Option): Unit = ()

  test("funK infers type args") {
    assertCompiles(
      """|import tofu.syntax.funk._
         |infer1(funK(Some(_)))
         |""".stripMargin
    )
  }

  def infer2[F[_]](x: F ~> Option): Unit = ()

  test("funKFrom infers type args") {
    assertCompiles(
      """|import tofu.syntax.funk._
         |infer2(funKFrom[Id](Some(_)))
         |""".stripMargin
    )
  }
}
