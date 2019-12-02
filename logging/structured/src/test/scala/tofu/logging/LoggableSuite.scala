package tofu.logging

import cats.syntax.either._
import cats.syntax.monoid._
import tofu.logging.LoggableSuite.TestInt
import tofu.syntax.logRenderer._
import tofu.syntax.loggable._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LoggableSuite extends AnyFlatSpec with Matchers {

  implicit val testIntLoggable: Loggable[TestInt] = new Loggable[Int] {
    def fields[I, V, R, S](a: Int, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      i.sub("missing")(r.zero) |+|
        i.sub("sign")(
          (v: V) =>
            r.coalesce(
              v => v.whenVal(a < 0)(v.putString("negative")),
              v => v.whenVal(a > 0)(v.putString("positive")),
              v
          )) |+|
        r.sub("value", i)(_.putInt(a.toLong))
    def putValue[I, V, R, S](a: Int, v: V)(implicit r: LogRenderer[I, V, R, S]): S = v.putInt(a.toLong)
    def logShow(a: Int): String                                                    = a.toString
  }.contramap(_.value)

  def json[A](a: A)(implicit loggable: Loggable[A]) = TethysBuilder(a)

  "int custom logging" should "be correct" in {
    json(TestInt(1)) shouldBe """{"missing":null,"sign":"positive","value":1}"""
    json(TestInt(-1)) shouldBe """{"missing":null,"sign":"negative","value":-1}"""
  }

  implicit val testLoggableEither = Loggable.either[String, Int].named("kek")

  "either custom logging" should "handle left" in {
    json("lol".asLeft[Int]) shouldBe """{"kek":"lol"}"""
  }

  it should "handle right" in {
    json(1.asRight[String]) shouldBe """{"kek":1}"""
  }

  it should "show left correct" in {
    "lol".asLeft[Int].logShow shouldBe "lol"
  }

  it should "show right correct" in {
    1.asRight[String].logShow shouldBe "1"
  }

}

object LoggableSuite {
  final case class TestInt(value: Int)
}
