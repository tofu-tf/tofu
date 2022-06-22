package tofu.logging

import cats.instances.list._
import cats.instances.unit._
import tofu.syntax.monadic._
import cats.syntax.foldable._
import cats.syntax.traverse._

import scala.collection.mutable
import cats.{Applicative, Monoid}
import io.circe.Json

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

sealed trait LogTree {
  import LogTree._
  def json: TailRec[Json] = this match {
    case dict: LogDict        =>
      dict.getList.flatMap { _.traverse { case (name, t) => t.json.map(name -> _) }.map(Json.obj(_: _*)) }
    case LogArr(items)        => items.toList.traverse(_.json).map(Json.arr(_: _*))
    case value: LogParamValue => TailCalls.done(value.jsonVal)
  }
}

// pretty much unsafe mutable builder for circe.Json from logs
object LogTree extends LogBuilder[Json] {
  implicit def monoid: Monoid[Output] = Applicative.monoid

  type Output = TailRec[Unit]
  type ValRes = TailRec[Boolean]
  class Value(private var tree: LogTree) {
    def get                 = always(tree)
    def set(value: LogTree) = always { tree = value }
  }
  type Top = LogDict

  private def always[A](a: => A): TailRec[A] = TailCalls.tailcall(TailCalls.done(a))

  class LogDict(private val values: mutable.LinkedHashMap[String, LogTree]) extends LogTree {
    def add(name: String, tree: LogTree): TailRec[Unit] = always(values.update(name, tree))
    def getList: TailRec[List[(String, LogTree)]]       = always(values.toList)
  }

  final case class LogArr(values: Iterable[LogTree]) extends LogTree

  private val newdict = always(mutable.LinkedHashMap.empty[String, LogTree]).map(new LogDict(_))
  private val newtree = always(new Value(NullValue))

  val receiver: LogRenderer[LogDict, Value, Output, ValRes] = new LogRenderer[LogDict, Value, Output, ValRes] {

    def coalesce(f: Value => ValRes, g: Value => ValRes, v: Value): ValRes =
      f(v).flatMap(written => if (written) TailCalls.done(true) else g(v))
    def zero(v: Value): TailRec[Boolean]                                   = TailCalls.done(false)
    def noop(i: LogDict): TailRec[Unit]                                    = TailCalls.done(())
    def combine(x: TailRec[Unit], y: TailRec[Unit]): TailRec[Unit]         = x *> y
    def putValue(value: LogParamValue, input: Value): TailRec[Boolean]     = input.set(value) as true

    def sub(name: String, input: LogDict)(receive: Value => TailRec[Boolean]): TailRec[Unit] =
      for {
        t <- newtree
        _ <- receive(t)
        v <- t.get
        _ <- input.add(name, v)
      } yield ()

    def list(size: Int, input: Value)(receive: (Value, Int) => TailRec[Boolean]): TailRec[Boolean] =
      for {
        ts <- newtree.replicateA(size)
        _  <- ts.zipWithIndex.foldM(true) { case (_, (v, i)) => receive(v, i) }
        vs <- ts.traverse(_.get)
        _  <- input.set(LogArr(vs))
      } yield true
    def dict(input: Value)(receive: LogDict => TailRec[Unit]): TailRec[Boolean]                    =
      newdict.flatTap(receive).flatMap(input.set).as(true)
  }

  def buildJson(buildTree: LogDict => Output): TailRec[Json] = newdict flatTap buildTree flatMap (_.json)

  def make(f: LogDict => TailRec[Unit]): Json = buildJson(f).result
}

sealed trait LogParamValue extends LogTree {
  def value: Any

  def jsonVal: Json = this match {
    case StrValue(value)     => Json.fromString(value)
    case IntValue(value)     => Json.fromLong(value)
    case BigIntValue(value)  => Json.fromBigInt(value)
    case DecimalValue(value) => Json.fromBigDecimal(value)
    case FloatValue(value)   => Json.fromDoubleOrString(value)
    case BoolValue(value)    => Json.fromBoolean(value)
    case NullValue           => Json.Null
  }
}

final case class StrValue(value: String)         extends LogParamValue
final case class IntValue(value: Long)           extends LogParamValue
final case class BigIntValue(value: BigInt)      extends LogParamValue
final case class DecimalValue(value: BigDecimal) extends LogParamValue
final case class FloatValue(value: Double)       extends LogParamValue
final case class BoolValue(value: Boolean)       extends LogParamValue
case object NullValue                            extends LogParamValue {
  def value = null
}
object LogParamValue {
  def apply(x: Any): LogParamValue = x match {
    case null          => NullValue
    case x: String     => StrValue(x)
    case x: Char       => StrValue(x.toString)
    case x: Byte       => IntValue(x.toLong)
    case x: Short      => IntValue(x.toLong)
    case x: Int        => IntValue(x.toLong)
    case x: Long       => IntValue(x)
    case x: BigInt     => BigIntValue(x)
    case x: BigDecimal => DecimalValue(x)
    case x: Float      => FloatValue(x.toDouble)
    case x: Double     => FloatValue(x)
    case x: Boolean    => BoolValue(x)
    case _             => StrValue(x.toString)
  }

}
