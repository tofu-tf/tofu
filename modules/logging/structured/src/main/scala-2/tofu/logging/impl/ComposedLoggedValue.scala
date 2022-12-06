package tofu.logging.impl

import tofu.logging.{LogRenderer, LoggedValue}

/** This is supposed to be used to log several `LoggedValue` as if they were passed as arguments to the logging method.
  * E.g. to provide single `LoggedValue` into `ContextMarker`. Be careful: the resulting structured log may contain the
  * same fields.
  */
class ComposedLoggedValue(values: Iterable[LoggedValue]) extends LoggedValue {
  override def toString: String = values.map(_.toString).mkString(", ")

  override def logFields[I, V, @specialized R, @specialized M](input: I)(implicit r: LogRenderer[I, V, R, M]): R =
    values.foldLeft(r.noop(input)) { (acc, value) =>
      r.combine(acc, value.logFields(input))
    }
}
