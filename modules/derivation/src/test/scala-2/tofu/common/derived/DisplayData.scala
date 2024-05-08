package tofu.common.derived

import cats.Eval
import derevo.derive
import tofu.common.Display
import tofu.common.Display.Config

object DisplayData {
  @derive(display)
  case class Bar(value: Int, another: String)

  @derive(display)
  case class Foo(bar: Bar, field: Double, xs: List[Int])

  @derive(display)
  sealed trait FooBar
  object FooBar {
    case class Barn(i: Int)    extends FooBar
    case class Darn(j: Double) extends FooBar
  }

  case class Emptiness()
  object Emptiness {
    implicit val displayEmptiness: Display[Emptiness] =
      (_: Config, _: Emptiness) => Eval.now(Vector.empty[String])
  }

  @derive(display)
  case class Bjarn(i: Emptiness)

  case class Nested(a: Int, b: Int, c: Int)
  object Nested {
    implicit val displayNested: Display[Nested] =
      (_: Config, ns: Nested) => Eval.now(Vector("Nested {", s"a = ${ns.a}, ", s"b = ${ns.b}, ", s"c = ${ns.c}", "}"))
  }

  @derive(display)
  case class Cont(a: Int, b: Nested)

}
