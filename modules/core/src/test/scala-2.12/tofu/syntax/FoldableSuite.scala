package tofu.syntax

import cats.data.{Chain, Writer}
import cats.instances.stream._
import org.scalatest.flatspec.AnyFlatSpec
import tofu.syntax.foldable._
import tofu.syntax.monadic._

class FoldableSuite extends AnyFlatSpec {
  def add(s: Int, x: Int) = x > 0 whenOpt Writer.tell(Chain(s)).as(s + x)
  def look(x: Int)        = x > 0 whenOpt Writer.tell(Chain(x)).as(x.toString)
  val elems               = Stream.range(1, 10) #::: Stream.from(-1, -1)

  "foldWhileM" should "scan elements" in
    assert(
      elems.foldWhileM(0)(add).written.toList === List.range(1, 9).scanLeft(0)(_ + _)
    )

  "takeWhileM" should "filter elements" in
    assert(elems.takeWhileM(look).run === ((Chain.fromSeq(1 to 9), List.range(1, 10).map(_.toString))))
}
