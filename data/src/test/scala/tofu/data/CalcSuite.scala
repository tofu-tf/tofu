package tofu.data

import org.scalatest.flatspec.AnyFlatSpec
import cats.instances.all._
import tofu.data.calc.Translator

class CalcSuite extends AnyFlatSpec {

  type IntStreamT[+E, +A] = (Int, A)
  type IntStream[+A]      = ICalcM[IntStreamT, Any, Any, Nothing, A]

  def write(i: Int): IntStream[Unit] = CalcT.lift((i, ()))

  val simpleStream = for {
    _ <- write(1)
    _ <- write(2)
    _ <- write(3)
  } yield "ok"

  "streamy calc" should "calculate sum" in {
    val sum = simpleStream.trans
      .state[Int]
      .pure {
        case (i, a) => CalcM[Int].mapState(_ + i).as(a)
      }
      .supply(0)

    assert(sum.values === ((6, "ok")))
  }

  it should "aggregate items" in {
    val list = simpleStream.trans
      .state[Vector[Int]]
      .pure {
        case (i, a) => CalcM.write(Vector(i)).as(a)
      }
      .supply(Vector())

    assert(list.values === ((Vector(1, 2, 3), "ok")))
  }

  it should "aggregate concatenated streams" in {
    val items = Translator[IntStreamT, Any, Vector[Int]].pure {
      case (i, a) => CalcM.write(Vector(i)).as(a)
    }

    val list =
      (simpleStream >> simpleStream.trans.mapK[IntStreamT] { case (i, a) => (i + 3, a) })
        .translateForget(items)
        .supply(Vector())

    assert(list.values === ((Vector(1, 2, 3, 4, 5, 6), "ok")))
  }
}
