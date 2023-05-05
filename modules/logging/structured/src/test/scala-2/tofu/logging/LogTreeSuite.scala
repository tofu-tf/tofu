package tofu.logging

import cats.syntax.semigroup._
import org.scalatest.funsuite.AnyFunSuite
import tofu.logging.{LogTree, TethysBuilder}
import tofu.syntax.logRenderer._
class LogTreeSuite extends AnyFunSuite {

  case class Data(field1: String, field2: Int)

  object Data {
    implicit object loggable extends DictLoggable[Data] with ToStringLoggable[Data] {
      def fields[I, V, R, S](a: Data, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
        i.addString("field1", a.field1) |+|
          i.addInt("field2", a.field2.toLong)
    }

  }

  val value = Map("aaaa" -> List(Data("lol", 2), Data("kek", -1)))

  test("LogTree and tethys builder results are consistent") {
    assert(TethysBuilder(value) === LogTree(value).noSpaces)
  }
}
