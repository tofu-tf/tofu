package tofu.logging

import java.io.{PrintWriter, StringWriter}
import java.time.{Instant, LocalDate, LocalDateTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import alleycats.std.iterable._
import alleycats.std.set._
import cats.data._
import cats.instances.list._
import cats.instances.map._
import cats.instances.sortedSet._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.monoid._
import cats.syntax.show._
import cats.{Foldable, Show}
import simulacrum.typeclass
import tofu.control.Consume
import tofu.logging.Loggable.Base
import tofu.logging.impl._
import tofu.syntax.logRenderer._
import tofu.compat._
import tofu.compat.lazySeqInstances._

import scala.annotation.nowarn
import scala.collection.immutable.SortedSet
import scala.collection.{immutable, mutable}
import scala.concurrent.duration.FiniteDuration
import scala.{PartialFunction => PF, specialized => sp}

/** Typeclass for adding custom log values to message
  */
@typeclass @nowarn("cat=unused-imports")
trait Loggable[A] extends Loggable.Base[A] {

  /** same as this loggable, but do not show any info in the message string */
  def hide: Loggable[A] = new HiddenLoggable(this)

  /** combine two loggables: put fields of both, choose value of first that suits */
  def +(that: Loggable.Base[A]): Loggable[A] = that match {
    case EmptyLoggable => this
    case _             => new PlusLoggable[A](this, that)
  }

  def plus[B <: A](that: Loggable.Base[B]): Loggable.Base[B] = new PlusLoggable[B](this, that)

  /** log this value whenever predicate holds */
  def filter(p: A => Boolean): Loggable[A]      = new FilterLoggable[A](this, p)
  def filterC[B <: A](p: B => Boolean): Base[B] = new FilterLoggable[B](this, p)
  def contraCollect[B](f: B PF A): Loggable[B]  = contramap(f).filter(f.isDefinedAt)

  /** whenever fields  are called it would be a single field named `name` and corresponding value */
  def named(name: String): Loggable[A] = new NamedLoggable[A](name, this)

  def showInstance: Show[A] = logShow

  def narrow[B <: A]: Loggable[B] = this.asInstanceOf[Loggable[B]]
}

object Loggable {

  /** contravariant version of `Loggable` if one need it */
  @typeclass @nowarn("cat=unused-imports")
  trait Base[-A] {
    self =>

    def typeName: String  = ""
    def shortName: String = ""

    /** write all fields of current value in the current object - context */
    def fields[I, V, R, S](a: A, i: I)(implicit r: LogRenderer[I, V, R, S]): R

    /** put single logging field value */
    def putValue[I, V, R, S](a: A, v: V)(implicit r: LogRenderer[I, V, R, S]): S

    /** put single logging field value in the field with supplied name */
    def putField[I, V, R, S](a: A, name: String, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      r.sub(name, i)(putValue(a, _))

    /** add list of custom fields for value
      *
      * @param a        value for logging
      * @param addParam side-effectful function, adding custom field to log entry
      */
    def logVia(a: A, addParam: (String, Any) => Unit): Unit =
      fields(a: A, "")(LogRenderer.prefixed(addParam))

    /** display value in log message
      *
      * @param a value for logging
      * @return displayed form
      */
    def logShow(a: A): String

    /** Convert value to LoggedValue
      *
      * @param a value for logging
      * @return new Logged Value
      */
    def loggedValue(a: A): LoggedValue = new LoggedValue {
      override def logFields[I, V, @sp(Unit) R, @sp M](i: I)(implicit r: LogRenderer[I, V, R, M]): R =
        fields[I, V, R, M](a, i)

      override def putValue[I, V, R, S](v: V)(implicit r: LogRenderer[I, V, R, S]): S = self.putValue(a, v)

      override def putField[I, V, R, S](i: I, name: String)(implicit r: LogRenderer[I, V, R, S]): R =
        self.putField(a, name, i)

      override def toString: String = logShow(a)
      override def typeName: String = self.typeName
      def shortName: String         = self.shortName
    }

    /** transform input value befor logging */
    def contramap[B](f: B => A): Loggable[B] = new ContramapLoggable(this, f)

    /** drop the show value */
    def hide: Base[A]

    /** same as `Loggable.+` but contravariace-friendly version */
    def plus[B <: A](that: Base[B]): Base[B]

    /** same as `Loggable.fitler` but contravariance-friendly version */
    def filterC[B <: A](p: B => Boolean): Base[B]

    /** contravariant version of `collect` - log values of type `B` when they could be converted to `A` */
    def contraCollect[B](f: B PF A): Base[B]

    /** whenever full log is needed it would be an object with single field `name` and corresponding value */
    def named(name: String): Base[A]

    def showInstance: Show.ContravariantShow[A]

    def narrow[B <: A]: Loggable[B]
  }

  /** do nothing log */
  def empty[A]: SingleValueLoggable[A] = EmptyLoggable.narrow[A]

  /** put no field, not value, but render as Show string */
  def show[A: Show] = new SubLoggable[A] {
    override def putField[I, V, R, M](a: A, name: String, input: I)(implicit r: LogRenderer[I, V, R, M]): R = input.noop
    def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M                            = v.zero
    override def logShow(a: A): String                                                                      = a.show
  }

  /** choose appropriate value to log */
  def either[A: Loggable, B: Loggable]: Loggable[Either[A, B]] =
    Loggable[A].contraCollect[Either[A, B]] { case Left(a) => a } +
      Loggable[B].contraCollect { case Right(b) => b }

  final implicit val stringValue: Loggable[String]             = new SingleValueLoggable[String] {
    def logValue(a: String): LogParamValue = StrValue(a)
    override def putField[I, V, R, M](a: String, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R                                   =
      receiver.addString(name, a, input)
  }

  final implicit val byteLoggable: Loggable[Byte] = new SingleValueLoggable[Byte] {
    def logValue(a: Byte): LogParamValue                                                                              = IntValue(a.toLong)
    override def putField[I, V, R, M](a: Byte, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      receiver.addInt(name, a.toLong, input)
  }

  final implicit val shortLoggable: Loggable[Short] = new SingleValueLoggable[Short] {
    def logValue(a: Short): LogParamValue                                                                              = IntValue(a.toLong)
    override def putField[I, V, R, M](a: Short, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      receiver.addInt(name, a.toLong, input)
  }

  final implicit val intLoggable: Loggable[Int] = new SingleValueLoggable[Int] {
    def logValue(a: Int): LogParamValue                                                                              = IntValue(a.toLong)
    override def putField[I, V, R, M](a: Int, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      receiver.addInt(name, a.toLong, input)
  }

  final implicit val longLoggable: Loggable[Long] = new SingleValueLoggable[Long] {
    def logValue(a: Long): LogParamValue                                                                              = IntValue(a)
    override def putField[I, V, R, M](a: Long, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      receiver.addInt(name, a, input)
  }

  final implicit val bigIngLoggable: Loggable[BigInt] = new SingleValueLoggable[BigInt] {
    def logValue(a: BigInt): LogParamValue = BigIntValue(a)
    override def putField[I, V, R, M](a: BigInt, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R                                   =
      receiver.addBigInt(name, a, input)
  }

  final implicit val bigDecimalLoggable: Loggable[BigDecimal] = new SingleValueLoggable[BigDecimal] {
    def logValue(a: BigDecimal): LogParamValue = DecimalValue(a)
    override def putField[I, V, R, M](a: BigDecimal, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R                                       =
      receiver.addDecimal(name, a, input)
  }

  final implicit val floatLoggable: Loggable[Float] = new SingleValueLoggable[Float] {
    def logValue(a: Float): LogParamValue                                                                              = FloatValue(a.toDouble)
    override def putField[I, V, R, M](a: Float, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      receiver.addFloat(name, a.toDouble, input)
  }

  final implicit val doubleLoggable: Loggable[Double] = new SingleValueLoggable[Double] {
    def logValue(a: Double): LogParamValue = FloatValue(a)
    override def putField[I, V, R, M](a: Double, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R                                   =
      receiver.addFloat(name, a, input)
  }

  final implicit val booleanLoggable: Loggable[Boolean] = new SingleValueLoggable[Boolean] {
    def logValue(a: Boolean): LogParamValue = BoolValue(a)
    override def putField[I, V, R, M](a: Boolean, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R                                    =
      receiver.addBool(name, a, input)
  }

  private[this] def fldLoggable[T[x]: Foldable, A](implicit A: Loggable[A]): Loggable[T[A]] =
    new SubLoggable[T[A]] {
      def putValue[I, V, R, M](ta: T[A], v: V)(implicit r: LogRenderer[I, V, R, M]): M = {
        val arr = ta.foldLeft(mutable.Buffer.newBuilder[A])(_ += _).result()
        v.list(arr.size)((v, idx) => A.putValue(arr(idx), v))
      }
      def logShow(a: T[A]): String = {
        implicit val show: Show[A] = A.showInstance
        a.mkString_("[", ",", "]")
      }
    }
  final implicit def seqLoggable[A: Loggable]: Loggable[collection.Seq[A]]                  =
    fldLoggable[Iterable, A].contramap(_.toIterable)
  final implicit def immutableSeqLoggable[A: Loggable]: Loggable[immutable.Seq[A]]          =
    fldLoggable[Iterable, A].contramap(_.toIterable)

  final implicit def listLoggable[A: Loggable]: Loggable[List[A]]           = fldLoggable[List, A]
  final implicit def vectorLoggable[A: Loggable]: Loggable[Vector[A]]       = fldLoggable[Vector, A]
  final implicit def streamLoggable[A: Loggable]: Loggable[LazySeq[A]]      = fldLoggable[LazySeq, A]
  final implicit def chainLoggable[A: Loggable]: Loggable[Chain[A]]         = fldLoggable[Chain, A]
  final implicit def setLoggable[A: Loggable]: Loggable[Set[A]]             = fldLoggable[Set, A]
  final implicit def sortedSetLoggable[A: Loggable]: Loggable[SortedSet[A]] = fldLoggable[SortedSet, A]

  final implicit def nonEmptyListLoggable[A: Loggable]: Loggable[NonEmptyList[A]]     = fldLoggable[NonEmptyList, A]
  final implicit def nonEmptyVectorLoggable[A: Loggable]: Loggable[NonEmptyVector[A]] = fldLoggable[NonEmptyVector, A]
  final implicit def nonEmptyStreamLoggable[A: Loggable]: Loggable[NELazySeq[A]]      = fldLoggable[NELazySeq, A]
  final implicit def nonEmptyChainLoggable[A: Loggable]: Loggable[NonEmptyChain[A]]   = fldLoggable[NonEmptyChain, A]
  final implicit def nonEmptySetLoggable[A: Loggable]: Loggable[NonEmptySet[A]]       = fldLoggable[NonEmptySet, A]

  final implicit val instantLoggable: Loggable[Instant]                 = stringValue.contramap(_.toString)
  final implicit val zonedDateTimeLoggable: Loggable[ZonedDateTime]     = stringValue.contramap(_.toString)
  final implicit val offsetDateTimeLoggable: Loggable[OffsetDateTime]   = stringValue.contramap(_.toString)
  final implicit val localDateTimeLoggable: Loggable[LocalDateTime]     = stringValue.contramap(_.toString)
  final implicit val localDateLoggable: Loggable[LocalDate]             = stringValue.contramap(_.toString)
  final implicit val durationLoggable: Loggable[java.time.Duration]     = stringValue.contramap(_.toString)
  final implicit val uuidLoggable: Loggable[UUID]                       = stringValue.contramap(_.toString)
  final implicit val finiteDurationLoggable: Loggable[FiniteDuration]   = stringValue.contramap(_.toString)
  final implicit val sqlDateLoggable: Loggable[java.sql.Date]           = stringValue.contramap(_.toString)
  final implicit val sqlTimeLoggable: Loggable[java.sql.Time]           = stringValue.contramap(_.toString)
  final implicit val sqlTimestampLoggable: Loggable[java.sql.Timestamp] = stringValue.contramap(_.toString)

  final implicit def mapLoggable[A](implicit A: Loggable[A]): Loggable[Map[String, A]] =
    new DictLoggable[Map[String, A]] {
      implicit val ashow: Show[A]                                                             = A.showInstance
      def fields[I, V, R, M](a: Map[String, A], i: I)(implicit r: LogRenderer[I, V, R, M]): R =
        a.foldLeft(i.noop)((acc, kv) => acc |+| A.putField(kv._2, kv._1, i))
      def logShow(a: Map[String, A]): String                                                  = a.show
    }

  implicit val loggableInstance: Consume[Loggable] = new Consume[Loggable] {
    def empty[A]: Loggable[A]                                                  = Loggable.empty
    def combineK[A](x: Loggable[A], y: Loggable[A]): Loggable[A]               = x + y
    def contramap[A, B](fa: Loggable[A])(f: B => A): Loggable[B]               = fa.contramap(f)
    def switch[A, B](fa: Loggable[A], fb: Loggable[B]): Loggable[Either[A, B]] =
      Loggable.either[A, B](fa, fb)
  }

  final implicit def optLoggable[T](implicit loggable: Loggable[T]): Loggable[Option[T]] = new Loggable[Option[T]] {
    override def logVia(value: Option[T], f: (String, Any) => Unit): Unit = value.foreach(loggable.logVia(_, f))

    def putValue[I, V, R, M](oa: Option[T], v: V)(implicit r: LogRenderer[I, V, R, M]): M = oa match {
      case None    => v.zero
      case Some(a) => loggable.putValue(a, v)
    }

    def fields[I, V, R, @sp(Unit) M](oa: Option[T], i: I)(implicit receiver: LogRenderer[I, V, R, M]): R = oa match {
      case None    => i.noop
      case Some(a) => loggable.fields(a, i)
    }

    override def putField[I, V, R, M](oa: Option[T], name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R =
      oa match {
        case None    => input.noop
        case Some(a) => loggable.putField(a, name, input)
      }

    def logShow(a: Option[T]): String = a.fold("<none>")(loggable.logShow)
  }
}

/** specialized Loggable for multi-field objects */
trait DictLoggable[A] extends Loggable[A] {
  override def putField[I, V, R, M](a: A, name: String, input: I)(implicit r: LogRenderer[I, V, R, M]): R =
    r.subDict(name, input)(i1 => fields(a, i1))

  def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M = r.dict(v)(fields(a, _))
}

/** specialized loggable where value is rendered by `.toString` method */
trait ToStringLoggable[A] extends Loggable[A] {
  def logShow(a: A): String = a.toString
}

/** specialized loggable containing no fields, only suitable to be logged as part of something */
trait SubLoggable[A] extends Loggable[A] {
  def fields[I, V, R, M](a: A, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R = input.noop
}

/** specialized loggable that will not be rendered in the message */
trait HideLoggable[A] extends Loggable[A] {
  override def logShow(a: A): String = ""
}

/** specialized loggable that containing single value to log */
trait SingleValueLoggable[@specialized A] extends Loggable[A] with SubLoggable[A] {
  def logValue(a: A): LogParamValue

  /** to be redefined for primitives */
  override def putField[I, V, R, M](a: A, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    receiver.addField(name, logValue(a), input)

  override def logShow(a: A): String = a.toString

  override def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M =
    r.putValue(logValue(a), v)
}

trait LoggedValue {
  def typeName: String
  def shortName: String

  def logFields[I, V, @sp(Unit) R, @sp M](input: I)(implicit r: LogRenderer[I, V, R, M]): R
  def putValue[I, V, R, S](v: V)(implicit r: LogRenderer[I, V, R, S]): S               = r.dict(v)(logFields(_))
  def putField[I, V, R, S](i: I, name: String)(implicit r: LogRenderer[I, V, R, S]): R = r.sub(name, i)(putValue(_))

  def foreachLog(f: (String, Any) => Unit): Unit =
    logFields("")(LogRenderer.prefixed(f))
}

object LoggedValue {
  implicit val loggable: Loggable[LoggedValue] = new Loggable[LoggedValue] {
    def fields[I, V, @sp(Unit) R, M](a: LoggedValue, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      a.logFields(input)

    override def putValue[I, V, R, S](a: LoggedValue, v: V)(implicit r: LogRenderer[I, V, R, S]): S = a.putValue(v)

    override def putField[I, V, R, S](a: LoggedValue, name: String, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      a.putField(i, name)

    def logShow(a: LoggedValue): String = a.toString
  }

  implicit def loggableToLoggedValue[A](x: A)(implicit loggable: Loggable[A]): LoggedValue = loggable.loggedValue(x)

  def error(cause: Throwable): LoggedThrowable = new LoggedThrowable(cause)
}

final class LoggedThrowable(cause: Throwable) extends Throwable(cause.getMessage, cause) with LoggedValue {
  override def toString: String = cause.toString

  def logFields[I, V, @sp(Unit) R, @sp M](input: I)(implicit f: LogRenderer[I, V, R, M]): R = {
    val strWriter = new StringWriter()
    cause.printStackTrace(new PrintWriter(strWriter))
    f.addString("stacktrace", strWriter.toString, input)
  }

  override def typeName: String = cause.getClass.getTypeName
  def shortName: String         = "exception"
}
