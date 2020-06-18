package tofu

import cats.instances.either.catsStdInstancesForEither
import cats.syntax.either._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tofu.syntax.error._

class ErrorsSuite extends AnyFlatSpec with Matchers with EitherValues {

  "Raise" should "reRaise" in {
    val err: String = "oops"
    Raise[ErrorOr, String].reRaise(toErr[ErrorOr[Unit]](err)) shouldBe toErr[Unit](err)
  }

  "Errors" should "adoptError" in {
    val err: String                         = "oops"
    val pf: PartialFunction[String, String] = { case s if s == err => err + "_" + err }
    val fa: ErrorOr[Unit]                   = Raise[ErrorOr, String].raise(err)
    implicit val errors                     = Errors[ErrorOr, String]
    fa.adaptError(pf) shouldBe toErr(pf(err))
  }

  type ErrorOr[A] = Either[String, A]

  private def toErr[A](err: String): ErrorOr[A] = err.asLeft[A]
}
