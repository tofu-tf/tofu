package tofu.higherKind.derived
import derevo.derive
import tofu.higherKind.Embed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.OptionT
import EmbedSuite.Foo
import tofu.syntax.monadic._
import tofu.syntax.embed._
import cats.instances.either._
import cats.instances.option._
import cats.syntax.traverse._
import cats.syntax.either._
import scala.util.Try

class EmbedSuite extends AnyFlatSpec with Matchers {
  val checkingFoo1: Foo[Either[String, *]] = new Foo[Either[String, *]] {
    override def foo(x: Int, s: String): Either[String, Double] =
      Try(s.toDouble).toEither.left.map(_ => s"could not parse $s as double").map(_ * x)
    override def bar(a: List[Int]): Either[String, Unit]        =
      a.headOption.toRight("must contain at least one element").void
    def baz(a: List[String]): OptionT[Either[String, *], Unit]  =
      OptionT(a.headOption.traverse(_.asLeft[Unit]))
  }

  "embed" should "generate nice embed" in {
    val rightFoo = checkingFoo1.asRight[String].embed
    val leftFoo  = "failed".asLeft[Foo[Either[String, *]]].embed

    rightFoo.foo(2, "2.3") should ===(Right(4.6))
    rightFoo.foo(2, "fail") should ===(Left("could not parse fail as double"))

    rightFoo.bar(List(4, 5, 6)) should ===(Right(()))
    rightFoo.bar(List()) should ===(Left("must contain at least one element"))

    leftFoo.foo(2, "2.3") should ===(Left("failed"))
    leftFoo.foo(2, "fail") should ===(Left("failed"))

    leftFoo.bar(List(4, 5, 6)) should ===(Left("failed"))
    leftFoo.bar(List()) should ===(Left("failed"))
  }
}

object EmbedSuite {
  @derive(embed)
  trait Foo[F[_]] {
    def foo(x: Int, s: String): F[Double]
    def bar(a: List[Int]): F[Unit]
    def baz(a: List[String]): OptionT[F, Unit]
  }

  Embed[Foo]

}
