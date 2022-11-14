package tofu.logging

import cats.kernel.Semigroup
import tofu.logging.impl.ComposedLoggedValue

import scala.{specialized => sp}

trait LoggedValue {
  def typeName: String  = ""
  def shortName: String = ""

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

  implicit val loggedValueSemigroup: Semigroup[LoggedValue] = Semigroup.instance { (a, b) =>
    new ComposedLoggedValue(a :: b :: Nil)
  }
}

final class LoggedThrowable(cause: Throwable) extends Throwable(cause.getMessage, cause) with LoggedValue {
  override def toString: String = cause.toString

  def logFields[I, V, @sp(Unit) R, @sp M](input: I)(implicit f: LogRenderer[I, V, R, M]): R =
    Loggable.throwableLoggable.fields(cause, input)

  override def typeName: String  = cause.getClass.getTypeName
  override def shortName: String = "exception"
}
