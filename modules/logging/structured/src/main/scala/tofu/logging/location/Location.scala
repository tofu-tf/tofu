package tofu.logging.location

import tofu.logging.Loggable

case class Location(file: String, line: Int, applicationPoint: String)

object Location extends LocationMacroInstances {
  implicit val loggable: Loggable[Location] = Loggable[String].contramap { case Location(file, line, point) =>
    s"$point@($file:$line)"
  }
}
