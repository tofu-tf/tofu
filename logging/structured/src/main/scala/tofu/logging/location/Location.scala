package tofu.logging.location

import scala.reflect.macros.blackbox

case class Location(className: String, line: Int) {
  def loggerName: String = className
}

object Location {
  implicit def instance: Location =
    macro LocationMacro.impl

  object LocationMacro {
    def impl(c: blackbox.Context): c.Tree = {
      import c.universe._
      val file = c.enclosingPosition.source.path
        .replace("/", ".")
        .dropRight(6)
        .drop(1) //this is adhoc af and just for POC purpose
      val line = c.enclosingPosition.line
      q"_root_.tofu.logging.location.Location($file, $line)"
    }
  }
}
