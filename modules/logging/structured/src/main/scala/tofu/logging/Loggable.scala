package tofu.logging

import cats.Show
import cats.syntax.show._
import tofu.control.Consume
import tofu.internal.DataComp
import tofu.logging.Loggable.Base
import tofu.logging.impl._
import tofu.syntax.logRenderer._

import scala.annotation.unused
import scala.{PartialFunction => PF}

/** Typeclass for adding custom log values to message
  */
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

  /** something that works as [[named]] on the toplevel but ensures, that field is always represented as a singleton
    * dict inside other value
    */
  def singleton(name: String): Loggable[A] = new SingletonLoggable[A](name, this)

  def showInstance: Show[A] = (a: A) => logShow(a)

  def narrow[B <: A]: Loggable[B] = this.asInstanceOf[Loggable[B]]

  /** Creates a new `LogAnnotation` using this `Loggable` with provided name */
  def logAnnotation(name: String): LogAnnotation[A] =
    LogAnnotation.make(name)(this)
}

object Loggable extends LoggableInstances with DataComp[Loggable] {

  /** contravariant version of `Loggable` if one need it */
  trait Base[-A] {
    self =>

    def typeName: String  = ""
    def shortName: String = ""

    /** write all fields of current value in the current object - context */
    def fields[I, V, R, S](a: A, i: I)(implicit r: LogRenderer[I, V, R, S]): R

    /** put single logging field value */
    def putValue[I, V, R, S](a: A, v: V)(implicit r: LogRenderer[I, V, R, S]): S

    /** put single logging field value if it's convertible to string, hide it otherwise */
    def putMaskedValue[I, V, R, S](@unused a: A, v: V)(@unused f: String => String)(implicit
        r: LogRenderer[I, V, R, S]
    ): S = r.zero(v)

    /** put single logging field value in the field with supplied name */
    def putField[I, V, R, S](a: A, name: String, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      r.sub(name, i)(putValue(a, _))

    /** put single logging field value in the field with supplied name if it's convertible to string, hide it otherwise
      */
    def putMaskedField[I, V, R, S](a: A, name: String, i: I)(f: String => String)(implicit
        r: LogRenderer[I, V, R, S]
    ): R = r.sub(name, i)(putMaskedValue(a, _)(f))

    /** add list of custom fields for value
      *
      * @param a
      *   value for logging
      * @param addParam
      *   side-effectful function, adding custom field to log entry
      */
    def logVia(a: A, addParam: (String, Any) => Unit): Unit =
      fields(a: A, "")(LogRenderer.prefixed(addParam))

    /** display value in log message
      *
      * @param a
      *   value for logging
      * @return
      *   displayed form
      */
    def logShow(a: A): String

    /** Convert value to LoggedValue
      *
      * @param a
      *   value for logging
      * @return
      *   new Logged Value
      */
    def loggedValue(a: A): LoggedValue = new LoggedValue {
      override def logFields[I, V, @specialized(Unit) R, @specialized M](i: I)(implicit r: LogRenderer[I, V, R, M]): R =
        fields[I, V, R, M](a, i)

      override def putValue[I, V, R, S](v: V)(implicit r: LogRenderer[I, V, R, S]): S = self.putValue(a, v)

      override def putField[I, V, R, S](i: I, name: String)(implicit r: LogRenderer[I, V, R, S]): R =
        self.putField(a, name, i)

      override def toString: String  = logShow(a)
      override def typeName: String  = self.typeName
      override def shortName: String = self.shortName
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

    /** generate the combined value of that loggable and the other at the given position */
    def combinedValue[I, V, R, M, A1 <: A](a: A1, values: V, that: Base[A1])(implicit
        render: LogRenderer[I, V, R, M]
    ): M =
      render.coalesce(this.putValue(a, _), that.putValue(a, _), values)
  }

  object Base extends DataComp[Base]

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

  final implicit val loggableInstance: Consume[Loggable] = new Consume[Loggable] {
    def empty[A]: Loggable[A]                                                  = Loggable.empty
    def combineK[A](x: Loggable[A], y: Loggable[A]): Loggable[A]               = x + y
    def contramap[A, B](fa: Loggable[A])(f: B => A): Loggable[B]               = fa.contramap(f)
    def switch[A, B](fa: Loggable[A], fb: Loggable[B]): Loggable[Either[A, B]] =
      Loggable.either[A, B](fa, fb)
  }
}

/** specialized Loggable for multi-field objects */
trait DictLoggable[A] extends Loggable[A] {
  override def putField[I, V, R, M](a: A, name: String, input: I)(implicit r: LogRenderer[I, V, R, M]): R =
    r.subDict(name, input)(i1 => fields(a, i1))

  def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M = r.dict(v)(fields(a, _))

  override def combinedValue[I, V, R, M, A1 <: A](a: A1, v: V, that: Loggable.Base[A1])(implicit
      r: LogRenderer[I, V, R, M]
  ): M =
    r.dict(v)(i => r.combine(this.fields(a, i), that.fields(a, i)))
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

  override def putMaskedValue[I, V, R, S](a: A, v: V)(f: String => String)(implicit r: LogRenderer[I, V, R, S]): S =
    r.putString(f(a.toString), v)

  override def putMaskedField[I, V, R, S](a: A, name: String, i: I)(f: String => String)(implicit
      receiver: LogRenderer[I, V, R, S]
  ): R = receiver.addString(name, f(a.toString), i)
}
