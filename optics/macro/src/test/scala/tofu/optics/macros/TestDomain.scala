package tofu.optics.macros

sealed trait A
case object B extends A
case object C extends A

import tofu.optics.macros.{promote => prololo}
object A {
  val b = GenSubset[A, B.type]
}

case class Bar(i: Int)

case class Foo(b: Bar)

class FooField1
class FooField2

@Optics("contains_") case class FooBar(i: Int, j: String)
@ClassyOptics case class FooInner(field1: FooField1, field2: FooField2)
@ClassyOptics("contains_") case class FooBar2(i: Int, j: String, @prololo inner: FooInner)

@POptics("contains_") case class FooBar3[X](i: Int, j: String, x: X)
@ClassyPOptics("contains_") case class FooBar4[X](i: Int, j: String, x: X)
@ClassyOptics case class FooBar5[Y](i: Int, j: String, x: List[Y], @prololo inner: FooInner)
