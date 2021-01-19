package tofu.higherKind.derived

import RepresentableKSuite.{Foo}
import cats.data.{OptionT, Tuple2K}
import cats.instances.either._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.option._
import cats.tagless.syntax.functorK._
import cats.tagless.syntax.semigroupalK._
import cats.{Id, ~>}
import derevo.derive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tofu.data.Embedded
import tofu.syntax.embed._
import tofu.syntax.funk.funK

import scala.util.Try

class RepresentableKSuite extends AnyFlatSpec with Matchers {
  val checkingFoo: Foo[Either[String, *]] = new Foo[Either[String, *]] {
    override def foo(x: Int, s: String): Either[String, Double] =
      Try(s.toDouble).toEither.left.map(_ => s"could not parse $s as double").map(_ * x)
    override def bar(a: List[Int]): Either[String, Unit]        =
      a.headOption.toRight("must contain at least one element").void
    def baz(a: List[String]): OptionT[Either[String, *], Unit]  =
      OptionT(a.headOption.traverse(_.asLeft[Unit]))
  }

  val defaultFoo: Foo[Id] = new Foo[Id] {
    override def foo(x: Int, s: String): Double = x.toDouble
    override def bar(a: List[Int]): Unit        = ()
    def baz(a: List[String]): OptionT[Id, Unit] = OptionT.none
  }

  type MapR[+A] = Embedded[(String, +*), List, A]

  "representableK" should "generate nice mapK" in {
    val eitherToList: Either[String, *] ~> MapR = funK {
      case Left(err)  => Embedded(((err, Nil)))
      case Right(res) => Embedded((("", List(res))))
    }

    val mappedFoo: Foo[MapR] = checkingFoo.mapK(eitherToList)

    mappedFoo.foo(2, "2.3") should ===(Embedded(("", List(4.6))))
    mappedFoo.foo(2, "fail") should ===(Embedded(("could not parse fail as double", List())))

    mappedFoo.bar(List(4, 5, 6)) should ===(Embedded(("", List(()))))
    mappedFoo.bar(List()) should ===(Embedded(("must contain at least one element", List())))

    mappedFoo.baz(List("one", "two")) should ===(OptionT[MapR, Unit](Embedded(("one", Nil))))
    mappedFoo.baz(Nil) should ===(OptionT[MapR, Unit](Embedded(("", List(None)))))
  }

  it should "generate nice productK" in {
    val zippedFoo: Foo[Tuple2K[Either[String, *], Id, *]] = checkingFoo.productK(defaultFoo)

    def tuple[A](e: Either[String, A], a: A) = Tuple2K[Either[String, *], Id, A](e, a)

    zippedFoo.foo(2, "2.3") should ===(tuple(Right(4.6), 2))
    zippedFoo.foo(2, "fail") should ===(tuple(Left("could not parse fail as double"), 2))

    zippedFoo.bar(List(4, 5, 6)) should ===(tuple(Right(()), ()))
    zippedFoo.bar(List()) should ===(tuple(Left("must contain at least one element"), ()))

    zippedFoo.baz(List("one", "two")) should ===(OptionT(tuple(Left("one"), none[Unit])))
    zippedFoo.baz(Nil) should ===(OptionT(tuple(Right(none[Unit]), none[Unit])))

  }

  it should "generate nice embed" in {
    val rightFoo = checkingFoo.asRight[String].embed
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

object RepresentableKSuite {
  @derive(representableK)
  trait Foo[F[_]] {
    def foo(x: Int, s: String): F[Double]

    def bar(a: List[Int]): F[Unit]

    def baz(a: List[String]): OptionT[F, Unit]
  }
}
