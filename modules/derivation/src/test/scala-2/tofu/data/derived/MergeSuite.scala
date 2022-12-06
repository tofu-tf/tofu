package tofu.data.derived

import java.time.LocalDate

import cats.syntax.option._
import derevo.derive
import MergeSuite.{Bar, Foo}
import org.scalatest.flatspec.AnyFlatSpec
import Merge.ops._

class MergeSuite extends AnyFlatSpec {
  "simple merge" should "prefer left value" in {
    assert(Merge[Int].merge(1, 2) === 1)
    assert(1.merge(2) === 1)
  }

  "option merge" should "prefer left some value" in {
    assert(Merge[Option[Int]].merge(1.some, 2.some) === 1.some)
  }

  it should "prefer non-opt value" in {
    assert(Merge[Option[Int]].merge(None, 2.some) === 2.some)
    assert(Merge[Option[Int]].merge(1.some, None) === 1.some)

  }

  it should "prefer have no choice where it's none" in {
    assert(Merge[Option[Int]].merge(None, None) === None)
  }

  "flat case class" should "prefer left values" in {
    assert(
      Merge[Foo].merge(
        Foo(1, "1".some, 1.0.some),
        Foo(2, "2".some, 2.0.some)
      ) === Foo(1, "1".some, 1.0.some)
    )
  }

  it should "replace missing values" in {
    assert(
      Merge[Foo].merge(
        Foo(1, None, None),
        Foo(2, "2".some, 2.0.some)
      ) === Foo(1, "2".some, 2.0.some)
    )

    assert(
      Merge[Foo].merge(
        Foo(1, "1".some, None),
        Foo(2, "2".some, 2.0.some)
      ) === Foo(1, "1".some, 2.0.some)
    )

    assert(
      Merge[Foo].merge(
        Foo(1, None, 1.0.some),
        Foo(2, "2".some, 2.0.some)
      ) === Foo(1, "2".some, 1.0.some)
    )
  }

  it should "merge somes" in {
    assert(
      Merge[Option[Foo]].merge(
        Foo(1, None, None).some,
        Foo(2, "2".some, 2.0.some).some
      ) === Foo(1, "2".some, 2.0.some).some
    )

    assert(
      Merge[Option[Foo]].merge(
        Foo(1, "1".some, None).some,
        Foo(2, "2".some, 2.0.some).some
      ) === Foo(1, "1".some, 2.0.some).some
    )

    assert(
      Merge[Option[Foo]].merge(
        Foo(1, None, 1.0.some).some,
        Foo(2, "2".some, 2.0.some).some
      ) === Foo(1, "2".some, 1.0.some).some
    )
  }

  it should "prefer some" in {
    assert(
      Merge[Option[Foo]].merge(
        Foo(1, None, None).some,
        None
      ) === Foo(1, None, None).some
    )

    assert(
      Merge[Option[Foo]].merge(
        None,
        Foo(2, "2".some, 2.0.some).some
      ) === Foo(2, "2".some, 2.0.some).some
    )
  }

  "tuples" should "be merged" in {
    assert(
      Merge[(Int, Option[String])].merge(
        (1, "1".some),
        (2, "2".some)
      ) === ((1, "1".some))
    )

    assert(
      Merge[(Int, Option[String])].merge(
        (1, None),
        (2, "2".some)
      ) === ((1, "2".some))
    )

    assert(
      Merge[(Int, Option[String])].merge(
        (1, None),
        (2, None)
      ) === ((1, None))
    )
  }

  "complex" should "prefer somes at high level" in {
    assert(
      Merge[Bar].merge(
        Bar(Foo(11, "11".some, 1.1.some), Foo(12, "12".some, 1.2.some).some),
        Bar(Foo(21, "21".some, 2.1.some), Foo(22, "22".some, 2.2.some).some),
      ) === Bar(Foo(11, "11".some, 1.1.some), Foo(12, "12".some, 1.2.some).some)
    )

    assert(
      Merge[Bar].merge(
        Bar(Foo(11, "11".some, 1.1.some), None),
        Bar(Foo(21, "21".some, 2.1.some), Foo(22, "22".some, 2.2.some).some),
      ) === Bar(Foo(11, "11".some, 1.1.some), Foo(22, "22".some, 2.2.some).some)
    )

    assert(
      Merge[Bar].merge(
        Bar(Foo(11, "11".some, 1.1.some), Foo(12, "12".some, 1.2.some).some),
        Bar(Foo(21, "21".some, 2.1.some), None),
      ) === Bar(Foo(11, "11".some, 1.1.some), Foo(12, "12".some, 1.2.some).some)
    )
  }

  "complex" should "prefer somes in inner values" in {
    assert(
      Merge[Bar].merge(
        Bar(Foo(11, None, 1.1.some), Foo(12, "12".some, None).some),
        Bar(Foo(21, "21".some, 2.1.some), Foo(22, "22".some, 2.2.some).some),
      ) === Bar(Foo(11, "21".some, 1.1.some), Foo(12, "12".some, 2.2.some).some)
    )

    assert(
      Merge[Bar].merge(
        Bar(Foo(11, None, None), None),
        Bar(Foo(21, "21".some, 2.1.some), Foo(22, "22".some, 2.2.some).some),
      ) === Bar(Foo(11, "21".some, 2.1.some), Foo(22, "22".some, 2.2.some).some)
    )

    assert(
      Merge[Bar].merge(
        Bar(Foo(11, "11".some, None), Foo(12, None, 1.2.some).some),
        Bar(Foo(21, "21".some, 2.1.some), None),
      ) === Bar(Foo(11, "11".some, 2.1.some), Foo(12, None, 1.2.some).some)
    )
  }

  "local date" should "have merge instance" in {
    assert(
      Merge[LocalDate].merge(
        LocalDate.ofYearDay(1999, 256),
        LocalDate.ofYearDay(2000, 128)
      ) === LocalDate.ofYearDay(1999, 256),
    )
  }

}

object MergeSuite {
  @derive(Merge)
  final case class Foo(a: Int, b: Option[String], c: Option[Double])

  @derive(Merge)
  final case class Bar(x: Foo, y: Option[Foo])
}
