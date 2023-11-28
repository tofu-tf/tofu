package tofu.logging.impl

import tofu.logging._
import tofu.syntax.logRenderer._

class EmptyLoggable[A] extends SingleValueLoggable[A] {
  def logValue(a: A): LogParamValue                                                                              = NullValue
  override def putField[I, V, R, M](a: A, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    input.noop
  override def logShow(a: A): String                                                                             = ""

  override def hide: Loggable[A] = this

  override def +(that: Loggable.Base[A]): Loggable[A] = that.narrow

  override def plus[B <: A](that: Loggable.Base[B]): Loggable.Base[B] = that.narrow

  override def filter(p: A => Boolean): Loggable[A] = this

  override def filterC[B <: A](p: B => Boolean): Loggable.Base[B] = this

  override def contraCollect[B](f: PartialFunction[B, A]): Loggable[B] = EmptyLoggable.narrow[B]

  override def named(name: String): Loggable[A] = this

  override def narrow[B <: A]: EmptyLoggable[B] = this.asInstanceOf[EmptyLoggable[B]]
}

object EmptyLoggable extends EmptyLoggable[Any]
