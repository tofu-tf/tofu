package tofu.logging

import tofu.logging._
import cats.syntax.semigroup._
import org.scalatest.flatspec.AnyFlatSpec

class NamedLoggableCompositionSuite extends AnyFlatSpec {

  def inspect(loggable: Loggable[String]) = TethysBuilder.apply[String]("")(loggable)

  val fooLogn = Loggable.stringValue.named("foo")
  val barLogn = Loggable.stringValue.named("bar")

  val logn1 = fooLogn + barLogn
  val logn2 = new DictLoggable[String] with HideLoggable[String] {
    def fields[I, V, R, S](a: String, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      fooLogn.fields(a, i) |+| barLogn.fields(a, i)
  }

  "named logs combined" should "genenerate two item dictionary" in {
    assert(inspect(logn1) === """{"foo":"","bar":""}""")
  }

  it should "generate single item dictionary on rename" in {
    assert(inspect(logn1.named("buz")) === """{"buz":""}""")
  }

  "named logs manually combined" should "generate two item dictionary" in {
    assert(inspect(logn2) === """{"foo":"","bar":""}""")
  }

  it should "generate deep dictionary on rename" in {
    assert(inspect(logn2.named("buz")) === """{"buz":{"foo":"","bar":""}}""")
  }

  val fooLogs = Loggable.stringValue.singleton("foo")
  val barLogs = Loggable.stringValue.singleton("bar")

  val logs1 = fooLogs + barLogs
  val logs2 = new DictLoggable[String] with HideLoggable[String] {
    def fields[I, V, R, S](a: String, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      fooLogs.fields(a, i) |+| barLogs.fields(a, i)
  }

  "singleton logs combined" should "genenerate two item dictionary" in {
    assert(inspect(logs1) === """{"foo":"","bar":""}""")
  }

  it should "generate single item dictionary on rename" in {
    assert(inspect(logs1.named("buz")) === """{"buz":{"foo":"","bar":""}}""")
  }

  "singleton logs manually combined" should "generate two item dictionary" in {
    assert(inspect(logs2) === """{"foo":"","bar":""}""")
  }

  it should "generate deep dictionary on rename" in {
    assert(inspect(logs2.named("buz")) === """{"buz":{"foo":"","bar":""}}""")
  }
}
