package tofu.logging
package impl

import cats.data.AndThen

class ContramapLoggable[A, B](val self: Loggable.Base[A], val f: B => A) extends Loggable[B] {
  override def fields[I, V, R, M](b: B, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    self.fields(f(b), input)

  override def putValue[I, V, R, M](b: B, v: V)(implicit r: LogRenderer[I, V, R, M]): M =
    self.putValue(f(b), v)

  override def putMaskedValue[I, V, R, M](b: B, v: V)(m: String => String)(implicit r: LogRenderer[I, V, R, M]): M =
    self.putMaskedValue(f(b), v)(m)

  override def putField[I, V, R, M](b: B, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    self.putField(f(b), name, input)

  override def putMaskedField[I, V, R, M](b: B, name: String, input: I)(m: String => String)(implicit
      receiver: LogRenderer[I, V, R, M]
  ): R = self.putMaskedField(f(b), name, input)(m)

  override def logVia(b: B, addParam: (String, Any) => Unit): Unit = self.logVia(f(b), addParam)
  override def logShow(b: B): String                               = self.logShow(f(b))
  override def contramap[C](g: C => B): Loggable[C]                = self.contramap(AndThen(g).andThen(f))
}
