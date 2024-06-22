package tofu.common.derived

import cats.Eval
import tofu.common.Display
import tofu.common.Display.Config

object DisplayData {

  case class Bar(value: Int, another: String) derives Display

  case class Foo(bar: Bar, field: Double, xs: List[Int]) derives Display

  sealed trait FooBar derives Display
  object FooBar {
    case class Barn(i: Int)    extends FooBar
    case class Darn(j: Double) extends FooBar
  }

  case class Emptiness()
  object Emptiness {
    implicit val displayEmptiness: Display[Emptiness] =
      (_: Config, _: Emptiness) => Eval.now(Vector.empty[String])
  }

  case class Bjarn(i: Emptiness) derives Display

  case class Nested(a: Int, b: Int, c: Int)
  object Nested {
    implicit val displayNested: Display[Nested] =
      (_: Config, ns: Nested) => Eval.now(Vector("Nested {", s"a = ${ns.a}, ", s"b = ${ns.b}, ", s"c = ${ns.c}", "}"))
  }

  case class Cont(a: Int, b: Nested) derives Display
}
