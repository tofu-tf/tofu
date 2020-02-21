package tofu.syntax

import org.scalatest.FlatSpec
import cats.data.Writer
import tofu.syntax.monadic._
import cats.data.Chain
import cats.Monad
import tofu.syntax.foldable._
import cats.instances.stream._
import cats.instances.int._

class FoldableSuite extends FlatSpec {
  def add(s: Int, x: Int) = x > 0 whenOpt Writer.tell(Chain(s)).as(s + x)
  def look(x: Int)        = x > 0 whenOpt Writer.tell(Chain(x)).as(x.toString)
  val elems               = Stream.range(1, 10) #::: Stream.from(-1, -1)

  "foldWhileM" should "scan elements" in
    assert(
      elems.foldWhileM(0)(add).written.toList === List.range(1, 9).scanLeft(0)(_ + _)
    )

  "takeWhileM" should "filter elements" in
    assert(elems.takeWhileM(look).run === (Chain.fromSeq(1 to 9), List.range(1, 10).map(_.toString)))
}
