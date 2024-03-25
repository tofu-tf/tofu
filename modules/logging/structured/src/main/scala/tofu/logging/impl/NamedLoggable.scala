package tofu.logging
package impl

class NamedLoggable[A](name: String, that: Loggable[A]) extends Loggable[A] {
  def fields[I, V, R, M](a: A, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R = putField(a, name, input)

  override def putField[I, V, R, M](a: A, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    that.putField(a, name, input)
  def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M                                   = that.putValue(a, v)
  override def named(name: String): Loggable[A]                                                                  = new NamedLoggable(name, that)
  override def logShow(a: A): String                                                                             = that.logShow(a)
}
