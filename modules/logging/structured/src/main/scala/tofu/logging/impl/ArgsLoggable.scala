package tofu.logging.impl

import tofu.logging.{LogRenderer, LoggedValue}

class ArgsLoggable(values: Seq[(String, LoggedValue)]) extends LoggedValue {
  override def shortName: String = "arguments"

  override def toString = values.map { case (name, value) => s"$name = $value" }.mkString("(", ", ", ")")

  def logFields[I, V, @specialized R, @specialized M](input: I)(implicit r: LogRenderer[I, V, R, M]): R = {
    values.foldLeft(r.noop(input)) { (res, p) => r.combine(res, r.field(p._1, input, p._2)) }
  }
}
