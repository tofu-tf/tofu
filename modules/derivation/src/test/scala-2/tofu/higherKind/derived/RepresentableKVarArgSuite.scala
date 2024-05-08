package tofu.higherKind.derived

import cats.data.Tuple2K
import cats.{Id, ~>}
import derevo.derive
import tofu.syntax.embed._
import cats.tagless.syntax.functorK._
import cats.tagless.syntax.semigroupalK._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tofu.higherKind.derived.RepresentableKVarArgSuite.Foo

class RepresentableKVarArgSuite extends AnyFlatSpec with Matchers {
  val ioFoo: Foo[Id] = new Foo[Id] {
    override def foo(args: Int*): Id[Int] = args.sum
  }

  val ioTransform: Id ~> Id = new (Id ~> Id) {
    override def apply[A](fa: Id[A]): Id[A] = fa
  }

  "representableK" should "correctly handle varargs with mapK" in {
    val mappedFoo: Foo[Id] = ioFoo.mapK(ioTransform)

    mappedFoo.foo(1, 2) should ===(3)
    mappedFoo.foo(1) should ===(1)
    mappedFoo.foo() should ===(0)
  }

  "representableK" should "correctly handle varargs with productK" in {
    val zippedFoo: Foo[Tuple2K[Id, Id, *]] = ioFoo.productK(ioFoo)

    def tuple[A](a: A): Tuple2K[Id, Id, A] = Tuple2K[Id, Id, A](a, a)

    zippedFoo.foo(1, 2) should ===(tuple(3))
    zippedFoo.foo(1) should ===(tuple(1))
    zippedFoo.foo() should ===(tuple(0))
  }

  "representableK" should "correctly handle varargs with embed" in {
    val foo: Foo[Id] = ioFoo.asInstanceOf[Id[Foo[Id]]].embed

    foo.foo(1, 2) should ===(3)
    foo.foo(1) should ===(1)
    foo.foo() should ===(0)
  }

}

object RepresentableKVarArgSuite {

  @derive(representableK)
  trait Foo[F[_]] {
    def foo(args: Int*): F[Int]
  }

}
