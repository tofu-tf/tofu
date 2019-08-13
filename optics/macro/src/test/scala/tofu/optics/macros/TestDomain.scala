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