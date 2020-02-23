package tofu.syntax

import cats.instances.option._
import cats.syntax.option._
import org.scalatest.FlatSpec
import tofu.Raise
import tofu.syntax.feither._

class FEitherSuite extends FlatSpec {

  //--Helpers------------------------------------

  implicit class SuiteIdOps[A](id: A) {
    def asRightS[L]: Option[Either[L, A]] = Some(Right(id))
    def asLeftS[R]: Option[Either[A, R]] = Some(Left(id))
  }

  def defaultRight: Option[Either[String, Int]] = 4.asRightS[String]
  def defaultLeft: Option[Either[Int, String]] = 4.asLeftS[String]

  def testRight: Option[Either[Int, String]] = "test".asRightS[Int]

  def boomRight[L, R]: Option[Right[L, R]] = Some(Right(sys.error("boom")))
  def boom[A]: Option[A] = Some(sys.error("boom"))

  def optionRaise[R] = new Raise[Option, R] {
    override def raise[A](err: R): Option[A] = None
  }

  //--Helpers end---------------------------------

  "EitherIdFOps#asRightF" should "return Right" in {
    assert(
      42.asRightF[Option, String] === Some(Right(42))
    )
  }

  "EitherIdFOps#asLeftF" should "return Left" in {
    assert(
      42.asLeftF[Option, String] === Some(Left(42))
    )
  }

  "EitherFObjectOps#condF" should "return Right and check lazyness" in {
    Either.condF(true, 42.some, boom) === Some(Right(42))
  }

  it should "return Left and check lazyness" in {
    Either.condF(false, boom, 43.some) === Some(Left(42))
  }

  "EitherFOps#orElseF" should "return self in case of empty second" in {
    assert(
      defaultRight.orElseF(None) === defaultRight
    )
  }

  it should "return self in case of non-empty second" in {
    assert(
      defaultRight.orElseF(5.asRightS[String]) === defaultRight
    )
  }

  it should "return arg in case of empty second" in {
    assert(
      defaultLeft.orElseF(None) === None
    )
  }

  it should "return arg in case non-of empty second" in {
    assert(
      defaultLeft.orElseF(testRight) === testRight
    )
  }

  it should "check lazyness" in {
    defaultRight.orElseF(boomRight) === defaultRight
  }

  "EitherFOps#getOrElseF" should "return self in case of empty second" in {
    assert(
      defaultRight.getOrElseF(None) === Some(4)
    )
  }

  it should "return self in case of non-empty second" in {
    assert(
      defaultRight.getOrElseF(Some(3)) === Some(4)
    )
  }

  it should "return arg" in {
    defaultLeft.getOrElseF(Some(5)) === Some(5)
  }

  it should "check lazyness" in {
    assert(
      defaultRight.getOrElseF(boom) === Some(4)
    )
  }

  "EitherFOps#catchAll" should "return self" in {
    defaultRight.catchAll(s => Some(s.length)) === Some(4)
  }

  it should "return arg" in {
    defaultLeft.catchAll(i => Some(i.toString)) === Some("4")
  }


  "EitherFOps#absolve" should "return self in case of Right" in {
    implicit def raise[T]: Raise[Option, T] = optionRaise[T]

    defaultRight.absolve === Some(4)
  }

  it should "return None in case of Left" in {
    implicit def raise[T]: Raise[Option, T] = optionRaise[T]

    defaultLeft.absolve === None
  }
}
