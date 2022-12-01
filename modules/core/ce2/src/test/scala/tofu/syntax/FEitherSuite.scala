package tofu.syntax

import cats.instances.list._
import cats.instances.option._
import cats.syntax.option.catsSyntaxOptionId
import cats.syntax.option.none
import cats.syntax.either._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tofu.Raise
import tofu.syntax.feither._

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.nowarn

@nowarn("cat=lint-infer-any")
class FEitherSuite extends AnyWordSpec with Matchers {

  // --Helpers------------------------------------

  implicit class SuiteIdOps[A](id: A) {
    def asRightS[L]: Option[Either[L, A]] = Some(Right(id))

    def asLeftS[R]: Option[Either[A, R]] = Some(Left(id))
  }

  def defaultRight: Option[Either[String, Int]] = 4.asRightS[String]

  def defaultLeft: Option[Either[Int, String]] = 4.asLeftS[String]

  def testRight: Option[Either[Int, String]] = "test".asRightS[Int]

  def boomRight[L, R]: Option[Right[L, R]] = Some(Right(sys.error("boom")))

  def boom[A]: Option[A] = Some(sys.error("boom"))

  def optionRaise[R]: Raise[Option, R] = new Raise[Option, R] {
    override def raise[A](err: R): Option[A] = None
  }

  // --Helpers end---------------------------------

  "EitherIdFOps#asRightF" should {
    "return Right" in {
      42.asRightF[Option, String] mustBe Some(Right(42))
    }
  }

  "EitherIdFOps#asLeftF" should {
    "return Left" in {
      42.asLeftF[Option, String] mustBe Some(Left(42))
    }
  }

  "EitherFObjectOps#condF" should {

    "return Right and check lazyness" in {
      Either.condF(true, 42.some, boom) mustBe Some(Right(42))
    }

    "return Left and check lazyness" in {
      Either.condF(false, boom, 43.some) mustBe Some(Left(43))
    }
  }

  "EitherFOps#orElseF" should {

    "return self in case of empty second" in {
      defaultRight.orElseF(None) mustBe defaultRight
    }

    "return self in case of non-empty second" in {
      defaultRight.orElseF(5.asRightS[String]) mustBe defaultRight
    }

    "return arg in case of empty second" in {
      defaultLeft.orElseF(None) mustBe None
    }

    "return arg in case non-of empty second" in {
      defaultLeft.orElseF(testRight) mustBe testRight
    }

    "check lazyness" in {
      defaultRight.orElseF(boomRight) mustBe defaultRight
    }
  }

  "EitherFOps#getOrElseF" should {

    "return self in case of empty second" in {
      defaultRight.getOrElseF(None) mustBe Some(4)
    }

    "return self in case of non-empty second" in {
      defaultRight.getOrElseF(Some(3)) mustBe Some(4)
    }

    "return arg" in {
      defaultLeft.getOrElseF(Some(5)) mustBe Some(5)
    }

    "check lazyness" in {
      defaultRight.getOrElseF(boom) mustBe Some(4)
    }
  }

  "EitherFOps#catchAll" should {

    "return self" in {
      defaultRight.catchAll(s => Some(s.length)) mustBe Some(4)
    }

    "return arg" in {
      defaultLeft.catchAll(i => Some(i.toString)) mustBe Some("4")
    }
  }

  "EitherFOps#absolve" should {

    "return self in case of Right" in {
      implicit def raise[T]: Raise[Option, T] = optionRaise[T]

      defaultRight.absolve mustBe Some(4)
    }

    "return None in case of Left" in {
      implicit def raise[T]: Raise[Option, T] = optionRaise[T]

      defaultLeft.absolve mustBe None
    }
  }

  "EitherFOps#reRaise" should {

    "return self in case of Right" in {
      implicit def raise[T]: Raise[Option, T] = optionRaise[T]

      defaultRight.reRaise mustBe Some(4)
    }

    "return None in case of Left" in {
      implicit def raise[T]: Raise[Option, T] = optionRaise[T]

      defaultLeft.reRaise mustBe None
    }
  }

  "EitherFOps#assocR" should {

    "return valid value in case of Right-Right" in {
      defaultRight.map(_.asRight[Char]).assocR mustBe Some(Right(4))
    }

    "return valid value in case of Right-Left" in {
      defaultLeft.map(_.asRight[Char]).assocR mustBe Some(Left(Right(4)))
    }

    "return valid value in case of Left" in {
      4.asLeft[Either[Char, Char]].some.assocR mustBe Some(Left(Left(4)))
    }
  }

  "EitherFOps#assocL" should {

    "return valid value in case of Left-Right" in {
      defaultRight.map(_.asLeft[Char]).assocL mustBe Some(Right(Left(4)))
    }

    "return valid value in case of Left-Left" in {
      defaultLeft.map(_.asLeft[Char]).assocL mustBe Some(Left(4))
    }

    "return valid value in case of Right" in {
      4.asRight[Either[Char, Char]].some.assocL mustBe Some(Right(Right(4)))
    }
  }

  "EitherFOps#mapF" should {

    "return valid value in case of Right" in {
      defaultRight.mapF(i => Some(i + 1)) mustBe Some(Right(5))
    }

    "return valid value in case of Left" in {
      defaultLeft.mapF(_.some) mustBe Some(Left(4))
    }

    "return valid value in case of base None" in {
      none[Either[String, Int]].mapF(_.some) mustBe None
    }

    "return valid valid in case map to None" in {
      defaultRight.mapF(_ => None) mustBe None
    }
  }

  "EitherFOps#tapF" should {

    "run in case of Right" in {
      val atom = new AtomicInteger(1)
      val init = defaultRight

      val result = init.tapF(i => Some(atom.addAndGet(i)))
      result mustBe init
      atom.get() mustBe 5

    }

    "not run in case of Left" in {
      val atom = new AtomicInteger(1)
      val init = defaultLeft

      val result = init.tapF(i => Some(atom.addAndGet(i.toInt)))
      result mustBe init
      atom.get() mustBe 1
    }

    "not run in case of base None" in {
      val atom = new AtomicInteger(1)
      val init = none[Either[String, Int]].mapF(_.some)

      val result = init.tapF(i => Some(atom.addAndGet(i)))
      result mustBe init
      atom.get() mustBe 1
    }

    "not run in case map to None" in {
      val atom = new AtomicInteger(1)
      val init = defaultRight

      val result = init.tapF(_ => None)
      result mustBe None
      atom.get() mustBe 1
    }
  }

  "EitherFOps#mapIn" should {

    "return valid value in case of Right" in {
      defaultRight.mapIn(_ + 1) mustBe Some(Right(5))
    }

    "return valid value in case of Left" in {
      defaultLeft.mapIn(_ + 1) mustBe Some(Left(4))
    }

    "return valid value in case of base None" in {
      none[Either[String, Int]].mapIn(_ + 1) mustBe None
    }
  }

  "EitherFOps#leftMapF" should {

    "return valid value in case of Left" in {
      defaultLeft.leftMapF(i => Some(i + 1)) mustBe Some(Left(5))
    }

    "return valid value in case of Right" in {
      defaultRight.leftMapF(_.some) mustBe Some(Right(4))
    }

    "return valid value in case of base None" in {
      none[Either[String, Int]].leftMapF(_.some) mustBe None
    }

    "return valid value in case map to None" in {
      defaultLeft.leftMapF(_ => None) mustBe None
    }
  }

  "EitherFOps#leftTapF" should {

    "run in case of Left" in {
      defaultLeft.leftMapF(i => Some(i + 1)) mustBe Some(Left(5))
      val atom = new AtomicInteger(1)
      val init = defaultLeft

      val result = init.leftTapF(i => Some(atom.addAndGet(i)))
      result mustBe init
      atom.get() mustBe 5
    }

    "not run in case of Right" in {
      defaultRight.leftMapF(_.some) mustBe Some(Right(4))
      val atom = new AtomicInteger(1)
      val init = defaultRight

      val result = init.leftTapF(i => Some(atom.addAndGet(i.toInt)))
      result mustBe init
      atom.get() mustBe 1

    }

    "not run in case of base None" in {
      val atom = new AtomicInteger(1)
      val init = none[Either[Int, String]]

      val result = init.leftTapF(i => Some(atom.addAndGet(i)))
      result mustBe init
      atom.get() mustBe 1
    }

    "not run in case map to None" in {
      val atom = new AtomicInteger(1)
      val init = defaultLeft

      val result = init.leftTapF(_ => None)
      result mustBe None
      atom.get() mustBe 1
    }
  }

  "EitherFOps#flatMapIn" should {

    "return Right in case of base Right and mapping" in {
      defaultRight.flatMapIn(i => Right(i + 1)) mustBe Some(Right(5))
    }

    "return valid value in case of base Right and mapping to Left" in {
      defaultRight.flatMapIn(i => Left(i.toString)) mustBe Some(Left("4"))
    }

    "return valid value in case of base Left" in {
      defaultLeft.flatMapIn(_.asRight) mustBe Some(Left(4))
    }

    "return valid value in case of base None" in {
      none[Either[String, Int]].flatMapIn(_.asRight) mustBe None
    }
  }

  "EitherFOps#leftFlatMapIn" should {

    "return Left in case of base Left and mapping" in {
      defaultLeft.leftFlatMapIn(i => Left(i + 1)) mustBe Some(Left(5))
    }

    "return valid value in case of base Left and mapping to Right" in {
      defaultLeft.leftFlatMapIn(i => Right(i.toString)) mustBe Some(Right("4"))
    }

    "return valid value in case of base Right" in {
      defaultRight.leftFlatMapIn(_.asLeft) mustBe Some(Right(4))
    }

    "return valid value in case of base None" in {
      none[Either[String, Int]].leftFlatMapIn(_.asRight) mustBe None
      none[Either[String, Int]].leftFlatMapIn(_.asLeft) mustBe None
    }
  }

  "EitherFOps#leftFlatMapF" should {

    "return Left in case of base Left and mapping" in {
      defaultLeft.leftFlatMapF(i => Some(Left(i + 1))) mustBe Some(Left(5))
    }

    "return valid value in case of base Left and mapping to Right" in {
      defaultLeft.leftFlatMapF(i => Some(Right(i.toString))) mustBe Some(Right("4"))
    }

    "return valid value in case of base Right" in {
      defaultRight.leftFlatMapF(v => Some(v.asLeft)) mustBe Some(Right(4))
      defaultRight.leftFlatMapF(_ => None) mustBe Some(Right(4))
    }

    "return valid value in case of base None" in {
      none[Either[String, Int]].leftFlatMapF(v => Some(v.asRight)) mustBe None
      none[Either[String, Int]].leftFlatMapF(v => Some(v.asLeft)) mustBe None
      none[Either[String, Int]].leftFlatMapF(_ => None) mustBe None
    }
  }

  "EitherFOps#swapF" should {

    "return Left" in {
      defaultRight.swapF mustBe Some(Left(4))
    }

    "return Right" in {
      defaultLeft.swapF mustBe Some(Right(4))
    }

    "return None" in {
      none[Either[String, Int]].swapF mustBe None
    }
  }

  "EitherFOps#doubleFlatMap" should {

    "return None" in {
      none[Either[String, Int]].doubleFlatMap(_ => None) mustBe None
      defaultRight.doubleFlatMap(_ => None) mustBe None
    }

    "return Right" in {
      defaultRight.doubleFlatMap(i => Some(Right(i + 1))) mustBe Some(Right(5))
    }

    "return Left" in {
      defaultRight.doubleFlatMap(i => Some(Left(i.toString))) mustBe Some(Left("4"))
      defaultLeft.doubleFlatMap(i => Some(Left(i.length))) mustBe Some(Left(4))
      defaultLeft.doubleFlatMap(i => Some(Right(i.length))) mustBe Some(Left(4))
      defaultLeft.doubleFlatMap(_ => None) mustBe Some(Left(4))
    }
  }

  "EitherFOps#ensure" should {

    "return None" in {
      none[Either[String, Int]].ensure(_ => true, "no") mustBe None
    }

    "return Left" in {
      defaultLeft.ensure(_ => true, "no") mustBe Some(Left(4))
      defaultRight.ensure(_ > 5, "no") mustBe Some(Left("no"))
    }

    "return Right" in {
      defaultRight.ensure(_ == 4, "no") mustBe Some(Right(4))
    }
  }

  "EitherFOps#ensureF" should {

    "return None" in {
      none[Either[String, Int]].ensureF(_ => Some(true), Some("no")) mustBe None
      defaultRight.ensureF(i => Some(i > 4), None) mustBe None
      defaultRight.ensureF(_ => None, None) mustBe None
    }

    "return Left" in {
      defaultLeft.ensureF(_ => Some(true), Some("no")) mustBe Some(Left(4))
      defaultRight.ensureF(i => Some(i > 5), Some("no")) mustBe Some(Left("no"))
    }

    "return Right" in {
      defaultRight.ensureF(i => Some(i == 4), Some("no")) mustBe Some(Right(4))
    }
  }

  "EitherFOps#traverseF" should {

    "return None" in {
      none[Either[String, Int]].traverseF(_.some) mustBe None
    }

    "return Some(None)" in {
      defaultRight.traverseF(_ => None) mustBe Some(None)
    }

    "return Right" in {
      defaultRight.traverseF(i => (i + 1).some) mustBe Some(Some(Right(5)))
    }

    "return Left" in {
      defaultLeft.traverseF(i => (i + 1).some) mustBe Some(Some(Left(4)))
      defaultLeft.traverseF(_ => None) mustBe Some(Some(Left(4)))
    }
  }

  "EitherFOps#traverseAll" should {

    "return List(None)" in {
      none[Either[String, Int]].traverseAll(List(_)) mustBe List(None)
    }

    "return Nil" in {
      defaultRight.traverseAll(_ => Nil) mustBe Nil
    }

    "return Right" in {
      defaultRight.traverseAll(i => List(i + 1)) mustBe List(Some(Right(5)))
    }

    "return Left" in {
      defaultLeft.traverseAll(i => List(i + 5)) mustBe List(Some(Left(4)))
      defaultLeft.traverseAll(_ => Nil) mustBe List(Some(Left(4)))
    }
  }

  "EitherFOps#leftTraverseF" should {

    "return None" in {
      none[Either[String, Int]].leftTraverseF(_.some) mustBe None
    }

    "return Some(None)" in {
      defaultLeft.leftTraverseF(_ => None) mustBe Some(None)
    }

    "return Right" in {
      defaultRight.leftTraverseF(i => (i + 1).some) mustBe Some(Some(Right(4)))
    }

    "return Left" in {
      defaultLeft.leftTraverseF(i => (i + 1).some) mustBe Some(Some(Left(5)))
      defaultLeft.leftTraverseF(_ => None) mustBe Some(None)
    }
  }

  "EitherFOps#leftTraverseAll" should {

    "return List(None)" in {
      none[Either[String, Int]].leftTraverseAll(List(_)) mustBe List(None)
    }

    "return Nil" in {
      defaultLeft.leftTraverseAll(_ => Nil) mustBe Nil

    }

    "return Right" in {
      defaultRight.leftTraverseAll(_ => Nil) mustBe List(Some(Right(4)))
      defaultRight.leftTraverseAll(i => List(i + 1)) mustBe List(Some(Right(4)))
    }

    "return Left" in {
      defaultLeft.leftTraverseAll(i => List(i + 1)) mustBe List(Some(Left(5)))
    }
  }

  "EitherFOps#productF" should {
    "return None" in {
      none[Either[String, Int]].productF(none[Either[String, Int]]) mustBe None
      none[Either[String, Int]].productF(defaultRight) mustBe None
      none[Either[String, Int]].productF(defaultLeft) mustBe None
      defaultRight.productF(None) mustBe None
    }

    "return Left" in {
      defaultLeft.productF(None) mustBe Some(Left(4))
      defaultLeft.productF(defaultRight) mustBe Some(Left(4))
      defaultLeft.productF(defaultLeft.map(_.map(_ + 1))) mustBe Some(Left(4))
      defaultRight.productF(defaultLeft) mustBe Some(Left(4))
    }

    "return Right" in {
      defaultRight.productF(defaultRight.map(_.map(_ + 1))) mustBe Some(Right((4, 5)))
    }

    "check lazyness" in {
      defaultLeft.productF(boomRight) mustBe Some(Left(4))
    }
  }

  "EitherFOps#productRF" should {
    "return None" in {
      none[Either[String, Int]].productRF(none[Either[String, Int]]) mustBe None
      none[Either[String, Int]].productRF(defaultRight) mustBe None
      none[Either[String, Int]].productRF(defaultLeft) mustBe None
      defaultRight.productRF(None) mustBe None
    }

    "return Left" in {
      defaultLeft.productF(None) mustBe Some(Left(4))
      defaultLeft.productRF(defaultRight) mustBe Some(Left(4))
      defaultLeft.productRF(defaultLeft.map(_.map(_ + 1))) mustBe Some(Left(4))
      defaultRight.productRF(defaultLeft) mustBe Some(Left(4))
    }

    "return Right" in {
      defaultRight.productRF(defaultRight.map(_.map(_ + 1))) mustBe Some(Right(5))
    }

    "check lazyness" in {
      defaultLeft.productRF(boomRight) mustBe Some(Left(4))
    }
  }

  "EitherFOps#productLF" should {
    "return None" in {
      none[Either[String, Int]].productLF(none[Either[String, Int]]) mustBe None
      none[Either[String, Int]].productLF(defaultRight) mustBe None
      none[Either[String, Int]].productLF(defaultLeft) mustBe None
      defaultRight.productLF(None) mustBe None
    }

    "return Left" in {
      defaultLeft.productF(None) mustBe Some(Left(4))
      defaultLeft.productLF(defaultRight) mustBe Some(Left(4))
      defaultLeft.productLF(defaultLeft.map(_.map(_ + 1))) mustBe Some(Left(4))
      defaultRight.productLF(defaultLeft) mustBe Some(Left(4))
    }

    "return Right" in {
      defaultRight.productLF(defaultRight.map(_.map(_ + 1))) mustBe Some(Right(4))
    }

    "check lazyness" in {
      defaultLeft.productLF(boomRight) mustBe Some(Left(4))
    }
  }

  "EitherFOps#apF" should {

    def defaultRightFunc = defaultRight.map(_.map(i => (l: Int) => i + l))
    def defaultLeftFunc  = defaultLeft.map(_.map(i => (l: Int) => i + l))
    def defaultLeftLFunc = defaultLeft.map(_.map(i => (l: String) => i + l.length))

    "return None" in {
      none[Either[String, Int => Int]].apF(none[Either[String, Int]]) mustBe None
      none[Either[String, Int => Int]].apF(defaultRight) mustBe None
      none[Either[String, String => Int]].apF(defaultLeft) mustBe None
      defaultRightFunc.apF(None) mustBe None
    }

    "return Left" in {
      defaultLeftFunc.apF(None) mustBe Some(Left(4))
      defaultLeftFunc.apF(defaultRight) mustBe Some(Left(4))
      defaultLeftLFunc.apF(defaultLeft) mustBe Some(Left(4))
      defaultLeftLFunc.apF(defaultLeft) mustBe Some(Left(4))
    }

    "return Right" in {
      defaultRightFunc.apF(defaultRight) mustBe Some(Right(8))
    }

    "check lazyness" in {
      defaultLeftFunc.apF(boomRight) mustBe Some(Left(4))
    }
  }

  "EitherFOps#map2F" should {

    "return None" in {
      none[Either[String, Int]].map2F(none[Either[String, Int]])(_ + _) mustBe None
      none[Either[String, Int]].map2F(defaultRight)(_ + _) mustBe None
      none[Either[Int, String]].map2F(defaultLeft)(_ + _) mustBe None
      defaultRight.map2F(none[Either[String, Int]])(_ + _) mustBe None
    }

    "return Left" in {
      defaultLeft.map2F(None)(_ + _) mustBe Some(Left(4))
      defaultLeft.map2F(defaultRight)(_ + _) mustBe Some(Left(4))
    }

    "return Right" in {
      defaultRight.map2F(defaultRight)(_ + _) mustBe Some(Right(8))
    }

    "check lazyness" in {
      defaultLeft.map2F(boomRight)(_ + _) mustBe Some(Left(4))
    }
  }

  "EitherFOps#flatMap2F" should {

    val sumF: (Int, Int) => Some[Int] = (a, b) => Some(a + b)

    "return None" in {
      none[Either[String, Int]].flatMap2F(none[Either[String, Int]])(sumF) mustBe None
      none[Either[String, Int]].flatMap2F(defaultRight)(sumF) mustBe None
      none[Either[Int, String]].flatMap2F(defaultLeft)((_, _) => Some("")) mustBe None
      defaultRight.flatMap2F(none[Either[String, Int]])(sumF) mustBe None
    }

    "return Left" in {
      defaultLeft.flatMap2F(None)((_, _: Any) => Some("")) mustBe Some(Left(4))
      defaultLeft.flatMap2F(defaultRight)((_, _) => Some("")) mustBe Some(Left(4))
    }

    "return Right" in {
      defaultRight.flatMap2F(defaultRight)(sumF) mustBe Some(Right(8))
    }

    "check lazyness" in {
      defaultLeft.flatMap2F(boomRight)((_, _: Any) => Some("")) mustBe Some(Left(4))
    }
  }

  "EitherFOps#mergeF" should {
    "return None" in {
      none[Either[Int, Int]].mergeF mustBe None
    }

    "return Left" in {
      defaultLeft.map(_.leftMap(_.toString)).mergeF mustBe Some("4")
    }

    "return Right" in {
      defaultRight.map(_.map(_.toString)).mergeF mustBe Some("4")
    }
  }

  "EitherIdFOps#rightIn" should {
    "return None" in {
      none[Int].rightIn[String] mustBe None
    }

    "return Right" in {
      4.some.rightIn mustBe Some(Right(4))
    }
  }

  "EitherIdFOps#leftIn" should {
    "return None" in {
      none[Int].leftIn mustBe None
    }

    "return Left" in {
      4.some.leftIn mustBe Some(Left(4))
    }
  }

}
