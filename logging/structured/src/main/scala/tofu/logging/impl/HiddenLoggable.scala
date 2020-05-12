package tofu.logging
package impl

class HiddenLoggable[A](val self: Loggable.Base[A]) extends Loggable[A] {
  override def logShow(a: A): String = ""

  override def putField[I, V, R, M](a: A, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    self.putField(a, name, input)
  def fields[I, V, R, M](a: A, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R                          = self.fields(a, input)
  def putValue[I, V, R, M](a: A, v: V)(implicit r: LogRenderer[I, V, R, M]): M                                   = self.putValue(a, v)
  override def logVia(a: A, addParam: (String, Any) => Unit): Unit                                               = self.logVia(a, addParam)
}
