package tofu.common

import scala.deriving.Mirror


package object derived {
      extension (x: Display.type) inline def derived[A](using Mirror.Of[A]): Display[A] = display.derived[A]

}
