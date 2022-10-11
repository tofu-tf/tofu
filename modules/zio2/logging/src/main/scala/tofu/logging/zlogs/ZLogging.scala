package tofu.logging.zlogs

import tofu.logging._
import zio._

object ZLogging {

  type Make = Logging.Make[UIO]

}
