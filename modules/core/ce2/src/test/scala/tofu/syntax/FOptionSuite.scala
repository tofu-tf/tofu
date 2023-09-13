package tofu.syntax

import cats.instances.list._
import cats.instances.either._
import org.scalatest.flatspec.AnyFlatSpec
import tofu.syntax.foption._
import tofu.syntax.feither._
import tofu.syntax.FOptionSuite._
import cats.syntax.either.catsSyntaxEitherId
import cats.syntax.option.none

import java.util.concurrent.atomic.AtomicInteger

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
      42.someF[Either[String, _]].orThrow("empty") === Right(42)
    )
  }

  "orThrow" should "raise error when empty" in {
    assert(
      noneF[Either[String, _], Int].orThrow("empty") === Left("empty")
    )
  }

  "semiflatMap" should "run when non empty" in {
    assert(
      42.someF[List].semiflatMap(x => List(x + 1)) === List(Some(43))
    )
  }

  "semiflatMap" should "keep empty when empty" in {
    assert(
      noneF[List, Int].semiflatMap(x => List(x + 1)) === List(None)
    )
  }

  "semiflatTap" should "run when non empty" in {
    val atom = new AtomicInteger(1)
    val init = 42.someF[List]

    val result = init.semiflatTap(x => List(atom.addAndGet(x)))

    assert(result === init)
    assert(atom.get() === 43)
  }

  "semiflatTap" should "keep value when empty" in {
    val atom = new AtomicInteger(1)
    val init = noneF[List, Int]

    val result = init.semiflatTap(x => List(atom.addAndGet(x)))

    assert(result === init)
    assert(atom.get() === 1)
  }

  "mapIn" should "run when non empty" in {
    assert(
      42.someF[List].mapIn(_ + 1) === List(Some(43))
    )
  }

  "mapIn" should "keep empty when empty" in {
    assert(
      noneF[List, Int].mapIn(_ + 1) === List(None)
    )
  }

  "flatMapIn" should "run when non empty" in {
    assert(
      42.someF[List].flatMapIn(v => Some(v + 1)) === List(Some(43))
    )
  }

  "flatMapIn" should "keep empty when empty" in {
    assert(
      noneF[List, Int].flatMapIn(v => Some(v + 1)) === List(None)
    )
    assert(
      List.empty[Option[Int]].flatMapIn(v => Some(v + 1)) === Nil
    )
  }

  "flatMapIn" should "return empty when empty is returned from function" in {
    assert(
      42.someF[List].flatMapIn(_ => None) === List(None)
    )
  }

  "doubleFlatMap" should "run when non empty" in {
    assert(
      42.someF[List].flatMapF(x => List(Some(x + 1))) === List(Some(43))
    )
  }

  "doubleFlatMap" should "keep empty when empty" in {
    assert(
      noneF[List, Int].flatMapF(x => List(Some(x + 1))) === List(None)
    )
  }

  "toRightF" should "return non-empty Left" in {
    assert(
      noneF[List, Int].toRightF(List("test")) === List(Left("test"))
    )
  }

  "toRightF" should "return Right" in {
    assert(
      42.someF[List].toRightF(List("test")) === List(Right(42))
    )
  }

  "toRightF" should "return empty M when empty M" in {
    assert(
      List.empty[Option[Int]].toRightF(List("test")) === Nil
    )
  }

  "toRightF" should "return empty Left when None" in {
    assert(
      noneF[List, Int].toRightF(List.empty[String]) === Nil
    )
  }

  "toRightF" should "check lazyness" in {
    assert(
      List.empty[Option[Int]].toRightF(boom) === Nil
    )
    assert(
      42.someF[List].toRightF(boom) === List(Right(42))
    )
  }

  "toRightIn" should "return non-empty Left" in {
    assert(
      noneF[List, Int].toRightIn("test") === List(Left("test"))
    )
  }

  "toRightIn" should "return Right" in {
    assert(
      42.someF[List].toRightIn("test") === List(Right(42))
    )
  }

  "toRightIn" should "return empty M when empty M" in {
    assert(
      List.empty[Option[Int]].toRightIn("test") === Nil
    )
  }

  "toRightIn" should "check lazyness" in {
    assert(
      List.empty[Option[Int]].toRightIn(boom) === Nil
    )
    assert(
      42.someF[List].toRightIn(boom) === List(Right(42))
    )
  }

  "toLeftF" should "return non-empty Right" in {
    assert(
      noneF[List, Int].toLeftF(List("test")) === List(Right("test"))
    )
  }

  "toLeftF" should "return Left" in {
    assert(
      42.someF[List].toLeftF(List("test")) === List(Left(42))
    )
  }

  "toLeftF" should "return Left when empty M" in {
    assert(
      List.empty[Option[Int]].toLeftF(List("test")) === Nil
    )
  }

  "toLeftF" should "return Right when None" in {
    assert(
      noneF[List, Int].toLeftF(List("test")) === List(Right("test"))
    )
  }

  "toLeftF" should "return empty Right when None and function return empty M" in {
    assert(
      noneF[List, Int].toLeftF(List.empty[String]) === Nil
    )
  }

  "toLeftF" should "check lazyness" in {
    assert(
      List.empty[Option[Int]].toLeftF(boom) === Nil
    )
    assert(
      42.someF[List].toLeftF(boom) === List(Left(42))
    )
  }

  "toLeftIn" should "return Left" in {
    assert(
      42.someF[List].toLeftIn("test") === List(Left(42))
    )
  }

  "toLeftIn" should "return Left when empty M" in {
    assert(
      List.empty[Option[Int]].toLeftIn(List("test")) === Nil
    )
  }

  "toLeftIn" should "return Right when None" in {
    assert(
      noneF[List, Int].toLeftIn("test") === List(Right("test"))
    )
  }

  "toLeftIn" should "check lazyness" in {
    assert(
      List.empty[Option[Int]].toLeftIn(boom) === Nil
    )
    assert(
      42.someF[List].toLeftIn(boom) === List(Left(42))
    )
  }

  "filterIn" should "return empty M" in {
    assert(
      List.empty[Option[Int]].filterIn(_ => true) === Nil
    )
  }

  "filterIn" should "return None if None" in {
    assert(
      noneF[List, Int].filterIn(_ => true) === noneF[List, Int]
    )
  }

  "filterIn" should "return valid value due the predicate" in {
    assert(
      42.someF[List].filterIn(_ == 42) === 42.someF[List]
    )
    assert(
      42.someF[List].filterIn(_ < 42) === noneF[List, Int]
    )
  }

  "filterF" should "return empty M" in {
    assert(
      List.empty[Option[Int]].filterF(_ => List(true)) === Nil
    )
    assert(
      List.empty[Option[Int]].filterF(_ => Nil) === Nil
    )
  }

  "filterF" should "return None if None" in {
    assert(
      noneF[List, Int].filterF(_ => List(true)) === noneF[List, Int]
    )
    assert(
      noneF[List, Int].filterF(_ => Nil) === noneF[List, Int]
    )
  }

  "filterF" should "return valid value due the predicate" in {
    assert(
      42.someF[List].filterF(i => List(i == 42)) === 42.someF[List]
    )
    assert(
      42.someF[List].filterF(i => List(i < 42)) === noneF[List, Int]
    )
    assert(
      42.someF[List].filterF(_ => Nil) === Nil
    )
  }

  "ensure" should "return err if None" in {
    assert(
      noneF[List, Int].ensure(_ => true, "err") === "err".asLeftF[List, Int]
    )
  }

  "ensure" should "return empty M" in {
    assert(
      List.empty[Option[Int]].ensure(_ => true, "err") === Nil
    )
  }

  "ensure" should "check lazyness" in {
    assert(
      List.empty[Option[Int]].ensure[String](_ => true, sys.error("boom")) === Nil
    )
  }

  "ensure" should "return valid value due the predicate" in {
    assert(
      42.someF[List].ensure(_ == 42, "err") === 42.asRightF[List, String]
    )
    assert(
      42.someF[List].ensure(_ < 42, "err") === "err".asLeftF[List, Int]
    )
  }

  "ensureF" should "return err if None" in {
    assert(
      noneF[List, Int].ensureF(_ => List(true), List("err")) === "err".asLeftF[List, Int]
    )
  }

  "ensureF" should "return empty M" in {
    assert(
      List.empty[Option[Int]].ensureF(_ => List(true), List("err")) === Nil
    )
    assert(
      42.someF[List].ensureF(_ => Nil, List.empty[String]) === Nil
    )
  }

  "ensureF" should "check lazyness" in {
    assert(
      List.empty[Option[Int]].ensureF[String](_ => List(true), sys.error("boom") :: Nil) === Nil
    )
  }

  "ensureF" should "return valid value due the predicate" in {
    assert(
      42.someF[List].ensureF(i => List(i == 42), List("err")) === 42.asRightF[List, String]
    )
    assert(
      42.someF[List].ensureF(i => List(i < 42), List("err")) === "err".asLeftF[List, Int]
    )
  }

  "collectF" should "return successful collected" in {
    assert(
      42.someF[List].collectF { case v => List(v + 1) } === List(Some(43))
    )
  }

  "collectF" should "return empty collected when empty M" in {
    assert(
      42.someF[List].collectF { case _ => List.empty[Int] } === Nil
    )
  }

  "collectF" should "return None collected when matching is failed" in {
    assert(
      42.someF[List].collectF { case 1 => List(1) } === List(none[Int])
    )
  }

  "collectF" should "return None collected when base is None" in {
    assert(
      noneF[List, Int].collectF { case 1 => List(1) } === List(none[Int])
    )
  }

  "collectIn" should "return successful collected" in {
    assert(
      42.someF[List].collectIn { case v => v + 1 } === List(Some(43))
    )
  }

  "collectIn" should "return None collected when matching is failed" in {
    assert(
      42.someF[List].collectIn { case 1 => 1 } === List(none[Int])
    )
  }

  "collectIn" should "return None collected when base is None" in {
    assert(
      noneF[List, Int].collectIn { case 1 => 1 } === List(none[Int])
    )
  }

  "traverseF" should "return Left" in {
    assert(
      42.someF[List].traverseF(_ => "err".asLeft[Int]) === List(Left("err"))
    )
  }

  "traverseF" should "return Right" in {
    assert(
      42.someF[List].traverseF(_.asRight[String]) === List(Right(Some(42)))
    )
  }

  "traverseF" should "return empty M" in {
    assert(
      List.empty[Option[Int]].traverseF(_.asRight[String]) === Nil
    )
    assert(
      List.empty[Option[Int]].traverseF(_.asLeft[String]) === Nil
    )
  }

  "traverseF" should "return err" in {
    assert(
      noneF[List, Int].traverseF(_.asLeft[String]) === List(Right(none[Int]))
    )
  }

  "traverseAll" should "return Left" in {
    assert(
      42.someF[List].traverseAll(_ => "err".asLeft[Int]) === Left("err")
    )
  }

  "traverseAll" should "return Right" in {
    assert(
      42.someF[List].traverseAll(_.asRight[String]) === Right(List(Some(42)))
    )
  }

  "traverseAll" should "return empty M" in {
    assert(
      List.empty[Option[Int]].traverseAll(_.asRight[String]) === Right(Nil)
    )
    assert(
      List.empty[Option[Int]].traverseAll(_.asLeft[String]) === Right(Nil)
    )
  }

  "traverseAll" should "return err" in {
    assert(
      noneF[List, Int].traverseAll(_.asLeft[String]) === Right(List(none[Int]))
    )
  }

  "productF" should "return None" in {
    assert(
      42.someF[List].productF(noneF[List, String]) === noneF[List, (Int, String)]
    )
    assert(
      noneF[List, Int].productF("err".someF[List]) === noneF[List, (Int, String)]
    )
    assert(
      noneF[List, Int].productF(noneF[List, String]) === noneF[List, (Int, String)]
    )
  }

  "productF" should "return Some" in {
    assert(
      42.someF[List].productF("test".someF[List]) === (42, "test").someF[List]
    )
  }

  "productF" should "return empty M" in {
    assert(
      List.empty[Option[Int]].productF("test".someF[List]) === Nil
    )
    assert(
      List.empty[Option[Int]].productF(noneF[List, String]) === Nil
    )
    assert(
      42.someF[List].productF(List.empty[Option[String]]) === Nil
    )
  }

  "productF" should "return M with None" in {
    assert(
      noneF[List, Int].productF(List.empty[Option[String]]) === List(None)
    )
  }

  "productF" should "check lazyness" in {
    assert(
      noneF[List, Int].productF(boom[String]) === List(None)
    )
    assert(
      List.empty[Option[Int]].productF(boom[String]) === Nil
    )
  }

  "productRF" should "return None" in {
    assert(
      42.someF[List].productRF(noneF[List, String]) === noneF[List, String]
    )
    assert(
      noneF[List, Int].productRF("err".someF[List]) === noneF[List, String]
    )
    assert(
      noneF[List, Int].productRF(noneF[List, String]) === noneF[List, String]
    )
  }

  "productRF" should "return Some" in {
    assert(
      42.someF[List].productRF("test".someF[List]) === "test".someF[List]
    )
  }

  "productRF" should "return empty M" in {
    assert(
      List.empty[Option[Int]].productRF("test".someF[List]) === Nil
    )
    assert(
      List.empty[Option[Int]].productRF(noneF[List, String]) === Nil
    )
    assert(
      42.someF[List].productRF(List.empty[Option[String]]) === Nil
    )
  }

  "productRF" should "return M with None" in {
    assert(
      noneF[List, Int].productRF(List.empty[Option[String]]) === List(None)
    )
  }

  "productRF" should "check lazyness" in {
    assert(
      noneF[List, Int].productRF(boom[String]) === List(None)
    )
    assert(
      List.empty[Option[Int]].productRF(boom[String]) === Nil
    )
  }

  "productLF" should "return None" in {
    assert(
      42.someF[List].productLF(noneF[List, String]) === noneF[List, Int]
    )
    assert(
      noneF[List, Int].productLF("err".someF[List]) === noneF[List, Int]
    )
    assert(
      noneF[List, Int].productLF(noneF[List, String]) === noneF[List, Int]
    )
  }

  "productLF" should "return Some" in {
    assert(
      42.someF[List].productLF("test".someF[List]) === 42.someF[List]
    )
  }

  "productLF" should "return empty M" in {
    assert(
      List.empty[Option[Int]].productLF("test".someF[List]) === Nil
    )
    assert(
      List.empty[Option[Int]].productLF(noneF[List, String]) === Nil
    )
    assert(
      42.someF[List].productLF(List.empty[Option[String]]) === Nil
    )
  }

  "productLF" should "return M with None" in {
    assert(
      noneF[List, Int].productLF(List.empty[Option[String]]) === List(None)
    )
  }

  "productLF" should "check lazyness" in {
    assert(
      noneF[List, Int].productLF(boom[String]) === List(None)
    )
    assert(
      List.empty[Option[Int]].productLF(boom[String]) === Nil
    )
  }

  "apF" should "return None" in {
    assert(
      ((_: Int) + 42).someF[List].apF(noneF[List, Int]) === noneF[List, Int]
    )
    assert(
      noneF[List, Int => Int].apF(42.someF[List]) === noneF[List, Int]
    )
    assert(
      noneF[List, Int => Int].apF(noneF[List, Int]) === noneF[List, Int]
    )
  }

  "apF" should "return Some" in {
    assert(
      ((_: Int) + 42).someF[List].apF(1.someF[List]) === 43.someF[List]
    )
  }

  "apF" should "return empty M" in {
    assert(
      List.empty[Option[Int => Int]].apF(0.someF[List]) === Nil
    )
    assert(
      List.empty[Option[Int => Int]].apF(noneF[List, Int]) === Nil
    )
    assert(
      ((_: Int) + 42).someF[List].apF(List.empty[Option[Int]]) === Nil
    )
  }

  "apF" should "return M with None" in {
    assert(
      noneF[List, Int => Int].apF(List.empty[Option[Int]]) === List(None)
    )
  }

  "apF" should "check lazyness" in {
    assert(
      noneF[List, Int => Int].apF(boom[Int]) === List(None)
    )
    assert(
      List.empty[Option[Int => Int]].apF(boom[Int]) === Nil
    )
  }

  "map2F" should "return None" in {
    assert(
      42.someF[List].map2F(noneF[List, Int])(_ + _) === noneF[List, Int]
    )
    assert(
      noneF[List, Int].map2F(42.someF[List])(_ + _) === noneF[List, Int]
    )
    assert(
      noneF[List, Int].map2F(noneF[List, Int])(_ + _) === noneF[List, Int]
    )
  }

  "map2F" should "return Some" in {
    assert(
      42.someF[List].map2F(1.someF[List])(_ + _) === 43.someF[List]
    )
  }

  "map2F" should "return empty M" in {
    assert(
      List.empty[Option[Int]].map2F(0.someF[List])(_ + _) === Nil
    )
    assert(
      List.empty[Option[Int]].map2F(noneF[List, Int])(_ + _) === Nil
    )
    assert(
      42.someF[List].map2F(List.empty[Option[Int]])(_ + _) === Nil
    )
  }

  "map2F" should "return M with None" in {
    assert(
      noneF[List, Int].map2F(List.empty[Option[Int]])(_ + _) === List(None)
    )
  }

  "map2F" should "check lazyness" in {
    assert(
      noneF[List, Int].map2F(boom[Int])(_ + _) === List(None)
    )
    assert(
      List.empty[Option[Int]].map2F(boom[Int])(_ + _) === Nil
    )
  }

  "flatMap2F" should "return None" in {
    assert(
      42.someF[List].flatMap2F(noneF[List, Int])((a, b) => List(a + b)) === noneF[List, Int]
    )
    assert(
      noneF[List, Int].flatMap2F(42.someF[List])((a, b) => List(a + b)) === noneF[List, Int]
    )
    assert(
      noneF[List, Int].flatMap2F(noneF[List, Int])((a, b) => List(a + b)) === noneF[List, Int]
    )
  }

  "flatMap2F" should "return Some" in {
    assert(
      42.someF[List].flatMap2F(1.someF[List])((a, b) => List(a + b)) === 43.someF[List]
    )
  }

  "flatMap2F" should "return empty M" in {
    assert(
      List.empty[Option[Int]].flatMap2F(0.someF[List])((a, b) => List(a + b)) === Nil
    )
    assert(
      List.empty[Option[Int]].flatMap2F(noneF[List, Int])((a, b) => List(a + b)) === Nil
    )
    assert(
      42.someF[List].flatMap2F(List.empty[Option[Int]])((a, b) => List(a + b)) === Nil
    )
    assert(
      List.empty[Option[Int]].flatMap2F(0.someF[List])((_, _) => Nil) === Nil
    )
    assert(
      List.empty[Option[Int]].flatMap2F(noneF[List, Int])((_, _) => Nil) === Nil
    )
    assert(
      42.someF[List].flatMap2F(List.empty[Option[Int]])((_, _) => Nil) === Nil
    )
  }

  "flatMap2F" should "return M with None" in {
    assert(
      noneF[List, Int].flatMap2F(List.empty[Option[Int]])((a, b) => List(a + b)) === List(None)
    )
  }

  "flatMap2F" should "check lazyness" in {
    assert(
      noneF[List, Int].flatMap2F(boom[Int])((_, _) => Nil) === List(None)
    )
    assert(
      List.empty[Option[Int]].flatMap2F(boom[Int])((_, _) => Nil) === Nil
    )
  }

  "someIn" should "return some in M (List)" in {
    assert(List(42).someIn === List(Some(42)))
  }
}

object FOptionSuite {
  def boom[B]: List[Option[B]] = sys.error("boom") :: Nil
}
