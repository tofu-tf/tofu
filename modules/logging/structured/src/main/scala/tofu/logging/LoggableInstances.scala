package tofu.logging

import java.time.{Instant, LocalDate, LocalDateTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import scala.collection.immutable.SortedSet
import scala.collection.{immutable, mutable}
import scala.concurrent.duration.FiniteDuration
import scala.{specialized => sp}

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
import tofu.compat._
import tofu.compat.lazySeqInstances._
import tofu.syntax.logRenderer._
import java.io.StringWriter
import java.io.PrintWriter

class LoggableInstances {
  final implicit val stringValue: Loggable[String] = new SingleValueLoggable[String] {
    def logValue(a: String): LogParamValue = StrValue(a)
    override def putField[I, V, R, M](a: String, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R =
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
    ): R =
      receiver.addBigInt(name, a, input)
  }

  final implicit val bigDecimalLoggable: Loggable[BigDecimal] = new SingleValueLoggable[BigDecimal] {
    def logValue(a: BigDecimal): LogParamValue = DecimalValue(a)
    override def putField[I, V, R, M](a: BigDecimal, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R =
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
    ): R =
      receiver.addFloat(name, a, input)
  }

  final implicit val booleanLoggable: Loggable[Boolean] = new SingleValueLoggable[Boolean] {
    def logValue(a: Boolean): LogParamValue = BoolValue(a)
    override def putField[I, V, R, M](a: Boolean, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R =
      receiver.addBool(name, a, input)
  }

  final implicit val unitLoggable: Loggable[Unit] = new SingleValueLoggable[Unit] {
    def logValue(a: Unit): LogParamValue = NullValue
    override def putField[I, V, R, M](a: Unit, name: String, input: I)(implicit
        receiver: LogRenderer[I, V, R, M]
    ): R = receiver.noop(input)
  }

  final implicit val nothingLoggable: Loggable[Nothing] = unitLoggable.narrow

  final implicit val throwableLoggable: Loggable[Throwable] = new Loggable[Throwable] {
    override def fields[I, V, R, S](cause: Throwable, i: I)(implicit r: LogRenderer[I, V, R, S]): R = {
      val strWriter = new StringWriter()
      cause.printStackTrace(new PrintWriter(strWriter))
      r.addString("stacktrace", strWriter.toString, i)
    }

    override def putValue[I, V, R, S](a: Throwable, v: V)(implicit r: LogRenderer[I, V, R, S]): S =
      r.putString(a.toString(), v)

    override def logShow(a: Throwable): String = a.toString
  }

  private[this] def fldLoggable[T[x]: Foldable, A](implicit A: Loggable[A]): Loggable[T[A]] =
    new SubLoggable[T[A]] {
      def putValue[I, V, R, M](ta: T[A], v: V)(implicit r: LogRenderer[I, V, R, M]): M = {
        val arr = ta.foldLeft(mutable.Buffer.newBuilder[A])(_ += _).result()
        v.list(arr.size)((v, idx) => A.putValue(arr(idx), v))
      }

      override def putMaskedValue[I, V, R, S](ta: T[A], v: V)(
          f: String => String
      )(implicit r: LogRenderer[I, V, R, S]): S = {
        val arr = ta.foldLeft(mutable.Buffer.newBuilder[A])(_ += _).result()
        v.list(arr.size)((v, idx) => A.putMaskedValue(arr(idx), v)(f))
      }

      def logShow(a: T[A]): String = {
        implicit val show: Show[A] = A.showInstance
        a.mkString_("[", ",", "]")
      }
    }
  final implicit def seqLoggable[A: Loggable]: Loggable[collection.Seq[A]]                  =
    fldLoggable[Iterable, A].narrow
  final implicit def immutableSeqLoggable[A: Loggable]: Loggable[immutable.Seq[A]]          =
    fldLoggable[Iterable, A].narrow

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

    override def putMaskedValue[I, V, R, S](a: Option[T], v: V)(
        f: String => String,
    )(implicit r: LogRenderer[I, V, R, S]): S = a match {
      case Some(value) => loggable.putMaskedValue(value, v)(f)
      case None        => v.zero
    }

    override def putMaskedField[I, V, R, S](a: Option[T], name: String, i: I)(
        f: String => String,
    )(implicit r: LogRenderer[I, V, R, S]): R = a match {
      case Some(value) => loggable.putMaskedField(value, name, i)(f)
      case None        => i.noop
    }
  }
}
