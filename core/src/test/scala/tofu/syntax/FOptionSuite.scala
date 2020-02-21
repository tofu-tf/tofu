package tofu.syntax

import cats.instances.list._
import cats.instances.either._
import org.scalatest.flatspec.AnyFlatSpec
import tofu.syntax.foption._

class FOptionSuite extends AnyFlatSpec {
  "getOrElseF" should "return inner value when non empty" in {
    assert(
      42.someF[List].getOrElseF(List(1, 1, 1)) === List(42)
    )
  }

  "getOrElseF" should "return argument when empty" in {
    assert(
      noneF[List, Int].getOrElseF(List(1, 1, 1)) === List(1, 1, 1)
    )
  }

  "orElseF" should "return self when non empty" in {
    assert(
      42.someF[List].orElseF(List[Option[Int]](Some(1), Some(1), Some(1))) === List(Some(42))
    )
  }

  "orElseF" should "return argument when empty" in {
    assert(
      noneF[List, Int].orElseF(List[Option[Int]](Some(1), Some(1), Some(1))) === List(Some(1), Some(1), Some(1))
    )
  }

  "orThrow" should "return inner value when non empty" in {
    assert(
      42.someF[Either[String, *]].orThrow("empty") === Right(42)
    )
  }

  "orThrow" should "raise error when empty" in {
    assert(
      noneF[Either[String, *], Int].orThrow("empty") === Left("empty")
    )
  }

  "flatMapOpt" should "run when non empty" in {
    assert(
      42.someF[List].flatMapOpt(x => List(x + 1)) === List(Some(43))
    )
  }

  "flatMapOpt" should "keep empty when empty" in {
    assert(
      noneF[List, Int].flatMapOpt(x => List(x + 1)) === List(None)
    )
  }

  "doubleFlatMap" should "run when non empty" in {
    assert(
      42.someF[List].doubleFlatMap(x => List(Some(x + 1))) === List(Some(43))
    )
  }

  "doubleFlatMap" should "keep empty when empty" in {
    assert(
      noneF[List, Int].doubleFlatMap(x => List(Some(x + 1))) === List(None)
    )
  }
}
