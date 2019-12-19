package tofu.optics.macros

sealed trait A
case object B extends A
case object C extends A

object A {
  val b = GenSubset[A, B.type]
}

case class Bar(i: Int)

case class Foo(b: Bar)

@Optics("contains_") case class FooBar(i: Int, j: String)
@ClassyOptics("contains_") case class FooBar2(i: Int, j: String)

@POptics("contains_") case class FooBar3[X](i: Int, j: String, x: X)
@ClassyPOptics("contains_") case class FooBar4[X](i: Int, j: String, x: X)
@ClassyOptics case class FooBar5[Y](i: Int, j: String, x: List[Y])
