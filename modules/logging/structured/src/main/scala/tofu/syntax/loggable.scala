package tofu
package syntax

import tofu.logging._

object loggable {
  implicit final class TofuLoggableOps[A](private val a: A) extends AnyVal {
    def fields[I, V, R, S](i: I)(implicit L: Loggable[A], r: LogRenderer[I, V, R, S]): R = L.fields(a, i)

    /** put single logging field value */
    def putValue[I, V, R, S](v: V)(implicit L: Loggable[A], r: LogRenderer[I, V, R, S]): S = L.putValue(a, v)

    /** put single logging field value in the field with supplied name */
    def putField[I, V, R, S](name: String, i: I)(implicit L: Loggable[A], r: LogRenderer[I, V, R, S]): R =
      L.putField(a, name, i)

    def logVia(addParam: (String, Any) => Unit)(implicit L: Loggable[A]): Unit = L.logVia(a, addParam)

    /** display value in log message */
    def logShow(implicit L: Loggable[A]): String = L.logShow(a)

    def loggedValue(implicit L: Loggable[A]): LoggedValue = L.loggedValue(a)
  }
}
