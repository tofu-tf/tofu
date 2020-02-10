package tofu.higherKind.derived

import RepresentableKSuite.Foo
import cats.data.{OptionT, Tuple2K}
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.functor._
import cats.tagless.syntax.functorK._
import cats.tagless.syntax.semigroupalK._
import cats.{Id, ~>}
import derevo.derive
import org.scalatest.{FlatSpec, Matchers}
import tofu.data.Embedded
import tofu.higherKind.{RepK, RepresentableK}
import tofu.syntax.embed._
import tofu.syntax.functionK.funK

import scala.util.Try

class RepresentableKSuite extends FlatSpec with Matchers {
  val checkingFoo: Foo[Either[String, *]] = new Foo[Either[String, *]] {
    override def foo(x: Int, s: String): Either[String, Double] =
      Try(s.toDouble).toEither.left.map(_ => s"could not parse $s as double").map(_ * x)
    override def bar(a: List[Int]): Either[String, Unit] =
      a.headOption.toRight("must contain at least one element").void
    def baz(a: List[Int]): OptionT[Either[String, *], Unit] = OptionT.liftF(Left("hello"))
  }

  val defaultFoo: Foo[Id] = new Foo[Id] {
    override def foo(x: Int, s: String): Double = x.toDouble
    override def bar(a: List[Int]): Unit        = ()
    def baz(a: List[Int]): OptionT[Id, Unit]    = OptionT.none
  }

  "representableK" should "generate nice mapK" in {
    val eitherToList: Either[String, *] ~> Embedded[(String, +*), List, *] = funK {
      case Left(err)  => Embedded(((err, Nil)))
      case Right(res) => Embedded((("", List(res))))
    }

    val mappedFoo = checkingFoo.mapK(eitherToList)

    mappedFoo.foo(2, "2.3") should ===(Embedded(("", List(4.6))))
    mappedFoo.foo(2, "fail") should ===(Embedded(("could not parse fail as double", List())))

    mappedFoo.bar(List(4, 5, 6)) should ===(Embedded(("", List(()))))
    mappedFoo.bar(List()) should ===(Embedded(("must contain at least one element", List())))
  }

  "representableK" should "generate nice productK" in {
    val zippedFoo = checkingFoo.productK(defaultFoo)

    def tuple[A](e: Either[String, A], a: A) = Tuple2K[Either[String, *], Id, A](e, a)

    zippedFoo.foo(2, "2.3") should ===(tuple(Right(4.6), 2))
    zippedFoo.foo(2, "fail") should ===(tuple(Left("could not parse fail as double"), 2))

    zippedFoo.bar(List(4, 5, 6)) should ===(tuple(Right(()), ()))
    zippedFoo.bar(List()) should ===(tuple(Left("must contain at least one element"), ()))
  }

  "representableK" should "generate nice embed" in {
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

    def baz(a: List[Int]): OptionT[F, Unit]
  }

  trait Foo1[F[_]] {
    def foo(x: Int, s: String): F[Double]

    def bar(a: List[Int]): F[Unit]

    def baz(a: List[Int]): OptionT[F, Unit]
  }

  new RepresentableK[Foo1] {
    def tabulate[F[_]](hom: RepK[Foo1, *] ~> F): Foo1[F] = new Foo1[F] {
      def foo(x: Int, s: String): F[Double] = hom(RepK.mk(_.foo(x, s)))
      def bar(a: List[Int]): F[Unit]        = hom(RepK.mk(_.bar(a)))
      def baz(a: List[Int]): OptionT[F, Unit] = {
        val OT = RepresentableK[OptionT[*[_], Unit]]
        OT.tabulate[F](funK(rep => hom(RepK[Foo1](foo => rep(foo.baz(a))))))
      }
    }
  }
}
