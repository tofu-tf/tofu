package tofu.logging
package impl

import tofu.syntax.logRenderer._

class FilterLoggable[A](that: Loggable.Base[A], p: A => Boolean) extends Loggable[A] {
  def fields[I, V, R, M](a: A, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    if (p(a)) that.fields(a, input) else input.noop

  def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M                                   =
    if (p(a)) that.putValue(a, v) else v.zero
  override def logVia(a: A, addParam: (String, Any) => Unit): Unit                                               = if (p(a)) that.logVia(a, addParam)
  override def putField[I, V, R, M](a: A, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    if (p(a)) that.putField(a, name, input) else receiver.noop(input)

  def logShow(a: A): String = if (p(a)) that.logShow(a) else ""
}
