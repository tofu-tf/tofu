package tofu.logging

import cats.effect.SyncIO
import cats.effect.concurrent.Ref
import cats.instances.list._
import cats.instances.unit._
import tofu.syntax.monadic._
import cats.syntax.foldable._
import cats.syntax.traverse._

import cats.{Applicative, Monoid}
import io.circe.Json

sealed trait LogTree {
  import LogTree._
  def json: SyncIO[Json] = this match {
    case LogDict(values)      =>
      values.get.flatMap { d => d.toList.traverse { case (name, t) => t.json.map(name -> _) }.map(Json.obj(_: _*)) }
    case LogArr(items)        => items.toList.traverse(_.json).map(Json.arr(_: _*))
    case value: LogParamValue => value.jsonVal.pure[SyncIO]
  }
}

object LogTree extends LogBuilder[Json] {
  implicit def monoid: Monoid[Output] = Applicative.monoid

  type Output = SyncIO[Unit]
  type ValRes = SyncIO[Boolean]
  type Value  = Ref[SyncIO, LogTree]
  type Top    = LogDict

  final case class LogDict(values: Ref[SyncIO, Map[String, LogTree]]) extends LogTree {
    def add(name: String, tree: LogTree): SyncIO[Unit] = values update (_ + (name -> tree))
  }

  final case class LogArr(values: Iterable[LogTree]) extends LogTree

  private val newdict = Ref[SyncIO].of(Map.empty[String, LogTree]).map(LogDict)
  private val newtree = Ref[SyncIO].of(NullValue: LogTree)

  val receiver: LogRenderer[LogDict, Value, Output, ValRes] = new LogRenderer[LogDict, Value, Output, ValRes] {

    def coalesce(f: Value => ValRes, g: Value => ValRes, v: Value): ValRes                                =
      f(v).flatMap(written => if (written) SyncIO(true) else g(v))
    def zero(v: Ref[SyncIO, LogTree]): SyncIO[Boolean]                                                    = SyncIO(false)
    def noop(i: LogDict): SyncIO[Unit]                                                                    = SyncIO.unit
    def combine(x: SyncIO[Unit], y: SyncIO[Unit]): SyncIO[Unit]                                           = x *> y
    def putValue(value: LogParamValue, input: Ref[SyncIO, LogTree]): SyncIO[Boolean]                      = input.set(value) as true
    def sub(name: String, input: LogDict)(receive: Ref[SyncIO, LogTree] => SyncIO[Boolean]): SyncIO[Unit] =
      for {
        t <- newtree
        _ <- receive(t)
        v <- t.get
        _ <- input.add(name, v)
      } yield ()

    def list(size: Int, input: Ref[SyncIO, LogTree])(receive: (Value, Int) => SyncIO[Boolean]): SyncIO[Boolean] =
      for {
        ts <- newtree.replicateA(size)
        _  <- ts.zipWithIndex.traverse_(receive.tupled)
        vs <- ts.traverse(_.get)
        _  <- input.set(LogArr(vs))
      } yield true
    def dict(input: Ref[SyncIO, LogTree])(receive: LogDict => SyncIO[Unit]): SyncIO[Boolean]                    =
      newdict flatMap input.set as true
  }

  def buildJson(buildTree: LogDict => Output): SyncIO[Json] = newdict flatTap buildTree flatMap (_.json)

  def make(f: LogDict => SyncIO[Unit]): Json = buildJson(f).unsafeRunSync()
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
