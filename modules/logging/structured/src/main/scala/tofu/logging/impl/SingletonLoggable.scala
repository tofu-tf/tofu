package tofu.logging
package impl

class SingletonLoggable[A](name: String, that: Loggable[A]) extends DictLoggable[A] {
  override def fields[I, V, R, M](a: A, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
    that.putField(a, name, input)

  override def logShow(a: A): String = s"{$name: ${that.logShow(a)}}"
}
