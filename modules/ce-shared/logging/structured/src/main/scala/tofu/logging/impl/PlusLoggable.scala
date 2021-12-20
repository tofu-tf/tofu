package tofu.logging
package impl

import cats.syntax.monoid._
import tofu.logging.Loggable.Base

class PlusLoggable[A](first: Base[A], second: Loggable.Base[A]) extends Loggable[A] {
  def fields[I, V, R, M](a: A, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    first.fields(a, input) |+| second.fields(a, input)

  def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M =
    first.combinedValue(a, v, second)

  override def logShow(a: A): String = {
    val s = first.logShow(a)
    if (s.nonEmpty) s else second.logShow(a)
  }
}
