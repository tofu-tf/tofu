package tofu.syntax

import cats.data.Ior
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.instances.option._
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.option.none
import tofu.syntax.fior._

class FIorSuite extends AnyWordSpec with Matchers  {

  def boom[A] = sys.error("boom")

  "TofuFIorOps#mapF" should {
    "run function well in case of right" in {
      Ior.right(1).some.mapF(i => Some(i + 1)) mustBe Some(Ior.right(2))
    }

    "run function well in case of left" in {
      Ior.left[String, Int]("").some.mapF(i => Some(i + 1)) mustBe Some(Ior.left(""))
    }

    "run function well in case of both" in {
      Ior.both("", 1).some.mapF(i => Some(i + 1)) mustBe Some(Ior.both("", 2))
    }

    "nothing happen in case of effect error" in {
      none[Ior[String, Int]].mapF(i => Some(i + 1)) mustBe none[Ior[String, Int]]
      Ior.right(1).some.mapF(_ => none[Int]) mustBe none[Ior[String, Int]]
      Ior.left[String, Int]("").some.mapF(_ => none[Int]) mustBe Some(Ior.left(""))
      Ior.both("", 1).some.mapF(_ => none[Int]) mustBe none[Ior[String, Int]]
    }

    "check lazyness" in {
      Ior.left[String, Int]("").some.mapF(_ => boom[Option[Int]]) mustBe Some(Ior.left(""))
    }
  }

  "TofuFIorOps#mapIn" should {
    "run function well in case of right" in {
      Ior.right(1).some.mapIn(_ + 1) mustBe Some(Ior.right(2))
    }

    "run function well in case of left" in {
      Ior.left[String, Int]("").some.mapIn(_ + 1) mustBe Some(Ior.left(""))
    }

    "run function well in case of both" in {
      Ior.both("", 1).some.mapIn(_ + 1) mustBe Some(Ior.both("", 2))
    }

    "nothing happen in case of effect error" in {
      none[Ior[String, Int]].mapIn(_ + 1) mustBe none[Ior[String, Int]]
    }
  }

  "TofuFIorOps#flatMapIn" should {
    "run function well in case of right" in {
      Ior.right(1).some.flatMapIn(i => Ior.right[String, Int](i + 1)) mustBe Some(Ior.right(2))
      Ior.right(1).some.flatMapIn(_ => Ior.left("")) mustBe Some(Ior.left(""))
      Ior.right(1).some.flatMapIn(i => Ior.both("", i + 1)) mustBe Some(Ior.both("", 2))
    }

    "run function well in case of left" in {
      Ior.left[String, Int]("").some.flatMapIn(i => Ior.right[String, Int](i + 1)) mustBe Some(Ior.left(""))
      Ior.left[String, Int]("").some.flatMapIn(_ => Ior.left("")) mustBe Some(Ior.left(""))
      Ior.left[String, Int]("foo").some.flatMapIn(i => Ior.both("bar", i + 1)) mustBe Some(Ior.left("foo"))
    }

    "run function well in case of both" in {
      Ior.both("foo", 1).some.flatMapIn(i => Ior.right[String, Int](i + 1)) mustBe Some(Ior.both("foo", 2))
      Ior.both("foo", 1).some.flatMapIn(_ => Ior.left("bar")) mustBe Some(Ior.left("foobar"))
      Ior.both("foo", 1).some.flatMapIn(i => Ior.both("bar", i + 1)) mustBe Some(Ior.both("foobar", 2))
    }

    "nothing happen in case of effect error" in {
      none[Ior[String, Int]].flatMapIn(i => Ior.right(i + 1)) mustBe none[Ior[String, Int]]
    }

    "check lazyness" in {
      Ior.left[String, Int]("").some.flatMapIn(_ => boom[Ior[String, Int]]) mustBe Some(Ior.left(""))
    }
  }

  "TofuFIorOps#doubleFlatMap" should {
    "run function well in case of right" in {
      Ior.right(1).some.doubleFlatMap(i => Some(Ior.right[String, Int](i + 1))) mustBe Some(Ior.right(2))
      Ior.right(1).some.doubleFlatMap(_ => Some(Ior.left(""))) mustBe Some(Ior.left(""))
      Ior.right(1).some.doubleFlatMap(i => Some(Ior.both("", i + 1))) mustBe Some(Ior.both("", 2))
    }

    "run function well in case of left" in {
      Ior.left[String, Int]("").some.doubleFlatMap(i => Some(Ior.right[String, Int](i + 1))) mustBe Some(Ior.left(""))
      Ior.left[String, Int]("").some.doubleFlatMap(_ => Some(Ior.left(""))) mustBe Some(Ior.left(""))
      Ior.left[String, Int]("foo").some.doubleFlatMap(i => Some(Ior.both("bar", i + 1))) mustBe Some(Ior.left("foo"))
    }

    "run function well in case of both" in {
      Ior.both("foo", 1).some.doubleFlatMap(i => Some(Ior.right[String, Int](i + 1))) mustBe Some(Ior.both("foo", 2))
      Ior.both("foo", 1).some.doubleFlatMap(_ => Some(Ior.left("bar"))) mustBe Some(Ior.left("foobar"))
      Ior.both("foo", 1).some.doubleFlatMap(i => Some(Ior.both("bar", i + 1))) mustBe Some(Ior.both("foobar", 2))
    }

    "nothing happen in case of effect error" in {
      none[Ior[String, Int]].doubleFlatMap(i => Some(Ior.right(i + 1))) mustBe none[Ior[String, Int]]
      Ior.right(1).some.doubleFlatMap(_ => none[Ior[String, Int]]) mustBe none[Ior[String, Int]]
      Ior.left[String, Int]("").some.doubleFlatMap(_ => none[Ior[String, Int]]) mustBe Some(Ior.left(""))
      Ior.both("", 1).some.doubleFlatMap(_ => none[Ior[String, Int]]) mustBe none[Ior[String, Int]]
    }

    "check lazyness" in {
      Ior.left[String, Int]("").some.doubleFlatMap(_ => boom[Option[Ior[String, Int]]]) mustBe Some(Ior.left(""))
    }
  }
}
