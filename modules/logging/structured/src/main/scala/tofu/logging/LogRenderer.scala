package tofu
package logging

import cats.Foldable
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.monoid._
import tofu.data.PArray
import syntax.logRenderer._
import scala.collection.compat._

import scala.{specialized => sp}

/** contextual log construction could serve as weak form of JSON building I and V could be the same type: prefix,
  * mutable builder, etc distinction is added to guide Loggable implementation for example, to work with unsafe builders
  * like Tethys Writer
  *
  * it would be great if we could use unique types `I` and `V` for each subcontext this would require however
  * polymorphic lambdas everywhere, so we are praise for user consciousness for not reusing `I` and `V` values in the
  * embedded scopes
  *
  * @tparam I
  *   some top level context, meaning we are inside dictionary
  * @tparam V
  *   some value level context, meanin we are building the value
  * @tparam R
  *   logging result on the top level
  * @tparam M
  *   logging result on the value level
  */
trait LogRenderer[I, V, @sp(Unit) R, @sp M] extends Semigroup[R] { self =>
  @inline implicit private[this] def thatRenderer: LogRenderer[I, V, R, M] = self

  def zero(v: V): M

  def noop(i: I): R

  def combine(x: R, y: R): R

  def coalesce(f: V => M, g: V => M, v: V): M

  def putValue(value: LogParamValue, v: V): M

  def sub(name: String, input: I)(receive: V => M): R

  def list(size: Int, input: V)(receive: (V, Int) => M): M

  def dict(input: V)(receive: I => R): M

  /** add new field to result */
  def addField(name: String, value: LogParamValue, input: I): R = sub(name, input)(putValue(value, _))

  /** focus on subelement of result */
  def subDict(path: String, input: I)(receive: I => R): R = sub(path, input)(dict(_)(receive))

  def field[A: Loggable](path: String, i: I, a: A): R = sub(path, i)(Loggable[A].putValue(a, _))

  /** add multiple dicts */
  def subDictList(path: String, size: Int, input: I)(receive: (I, Int) => R): R =
    sub(path, input)(list(size, _)((v, j) => dict(v)(i => receive(i, j))))

  def foldable[F[_]: Foldable, A](fa: F[A], v: V)(receive: (V, A) => M): M = {
    import tofu.data.PArray.ArrOps // for scala 2.11
    val arr = PArray.fromFoldable(fa)
    list(arr.length, v)((v, i) => receive(v, arr(i)))
  }

  def putFoldable[F[_]: Foldable, A: Loggable](fa: F[A], v: V): M =
    foldable(fa, v)((v, a) => Loggable[A].putValue(a, v))

  def coll[A](fa: IterableOnce[A], v: V)(receive: (V, A) => M): M = {
    import PArray.ArrOps // for scala 2.11
    val arr = PArray.fromColl(fa)
    list(arr.length, v)((v, i) => receive(v, arr(i)))
  }

  def putColl[A: Loggable](fa: IterableOnce[A], v: V): M =
    coll(fa, v)((v, a) => Loggable[A].putValue(a, v))

  def addString(name: String, value: String, input: I): R      = addField(name, StrValue(value), input)
  def addInt(name: String, value: Long, input: I): R           = addField(name, IntValue(value), input)
  def addFloat(name: String, value: Double, input: I): R       = addField(name, FloatValue(value), input)
  def addBigInt(name: String, value: BigInt, input: I): R      = addField(name, BigIntValue(value), input)
  def addDecimal(name: String, value: BigDecimal, input: I): R = addField(name, DecimalValue(value), input)
  def addBool(name: String, value: Boolean, input: I): R       = addField(name, BoolValue(value), input)

  def putString(value: String, input: V): M      = putValue(StrValue(value), input)
  def putInt(value: Long, input: V): M           = putValue(IntValue(value), input)
  def putFloat(value: Double, input: V): M       = putValue(FloatValue(value), input)
  def putBigInt(value: BigInt, input: V): M      = putValue(BigIntValue(value), input)
  def putDecimal(value: BigDecimal, input: V): M = putValue(DecimalValue(value), input)
  def putBool(value: Boolean, input: V): M       = putValue(BoolValue(value), input)

  def topFunctionMonoid: Monoid[I => R] = new Monoid[I => R] {
    def empty: I => R                         = self.noop
    def combine(x: I => R, y: I => R): I => R = i => self.combine(x(i), y(i))
  }

  def valueFunctionMonoid: Monoid[V => M] = new Monoid[V => M] {
    def empty: V => M                         = self.zero
    def combine(x: V => M, y: V => M): V => M = self.coalesce(x, y, _)
  }
}

/** contains some LogRenderer and therefore can accept loggable values and do something with them */
trait LogBuilder[U] {

  /** `I` of contained renderer */
  type Top

  /** `V` of contained renderer */
  type Value

  /** `M` of contained renderer */
  type ValRes

  /** `R` of contained renderer */
  @specialized(Unit) type Output

  implicit def receiver: LogRenderer[Top, Value, Output, ValRes]

  /** use renderer to produce the result */
  def make(f: Top => Output): U

  /** accept several loggable values and produce the result */
  def build[A](as: A*)(implicit L: Loggable[A]): U =
    make { d => as.foldLeft(d.noop)((acc, a) => acc |+| L.fields(a, d)) }

  /** accept loggable value and produce the result */
  def apply[A](a: A)(implicit L: Loggable[A]): U =
    make(d => L.fields(a, d))
}

object LogRenderer {
  private def join(pref: String, name: Any) = if (pref.isEmpty) name.toString else s"$pref-$name"

  abstract class LogRendererUnit[I, V, M] extends LogRenderer[I, V, Unit, M] {
    def noop(i: I): Unit                = ()
    def combine(x: Unit, y: Unit): Unit = ()
  }

  /** monomorphic context renderer */
  type Iso[I, R] = LogRenderer[I, I, R, R]

  abstract class IsoLogRendererUnit[A] extends LogRendererUnit[A, A, Unit] with Iso[A, Unit] {
    def zero(v: A): Unit                                 = ()
    def coalesce(f: A => Unit, g: A => Unit, v: A): Unit = f(v)
  }

  /** simple imperative renderer, log everything with accumulated prefix */
  def prefixed(f: (String, Any) => Unit): Iso[String, Unit] = new IsoLogRendererUnit[String] {
    def putValue(value: LogParamValue, input: String): Unit                  = f(input, value.value)
    def sub(name: String, input: String)(receive: String => Unit): Unit      = receive(join(input, name))
    def list(size: Int, input: String)(receive: (String, Int) => Unit): Unit =
      for (i <- 0 until size) receive(join(input, i), i)
    def dict(input: String)(receive: String => Unit): Unit                   = receive(input)
  }

  /** */
  final case class IdxPrefix(init: String, group: String, name: String) {
    def sub(s: String) = IdxPrefix(init, join(group, name), s)
    def idx(i: Int)    = IdxPrefix(join(init, join(group, name)), "", i.toString)
    def loc            = join(init, name)
  }

  def idxPrefixed(f: (String, Any) => Unit): IsoLogRendererUnit[IdxPrefix] = new IsoLogRendererUnit[IdxPrefix] {
    def putValue(value: LogParamValue, input: IdxPrefix): Unit                     = f(input.loc, value.value)
    def sub(name: String, input: IdxPrefix)(receive: IdxPrefix => Unit): Unit      = receive(input.sub(name))
    def list(size: Int, input: IdxPrefix)(receive: (IdxPrefix, Int) => Unit): Unit =
      for (i <- 0 until size) receive(input.idx(i), i)
    def dict(input: IdxPrefix)(receive: IdxPrefix => Unit): Unit                   = receive(input)
  }
}
