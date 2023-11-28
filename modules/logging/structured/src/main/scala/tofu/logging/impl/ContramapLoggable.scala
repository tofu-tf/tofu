package tofu.logging
package impl

import cats.data.AndThen

class ContramapLoggable[A, B](val self: Loggable.Base[A], val f: B => A) extends Loggable[B] {
  def fields[I, V, R, M](b: B, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R = self.fields(f(b), input)
  def putValue[I, V, R, M](a: B, v: V)(implicit r: LogRenderer[I, V, R, M]): M          = self.putValue(f(a), v)

  override def putField[I, V, R, M](a: B, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    self.putField(f(a), name, input)
  override def logVia(a: B, addParam: (String, Any) => Unit): Unit                                               = self.logVia(f(a), addParam)
  override def logShow(a: B): String                                                                             = self.logShow(f(a))
  override def contramap[C](g: C => B): Loggable[C]                                                              = self.contramap(AndThen(g).andThen(f))
}
