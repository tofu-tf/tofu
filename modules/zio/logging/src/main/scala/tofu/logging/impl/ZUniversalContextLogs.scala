package tofu.logging.impl

import tofu.logging.zlogs.ZLogging
import tofu.logging.{Loggable, LoggedValue}
import zio._

class ZUniversalContextLogs[R: Loggable] extends ZLogging.ZMake[R] {
  private def useContextValue(f: LoggedValue => Unit): URIO[R, Unit] = ZIO.access[R](f(_))

  override def byName(name: String): ZLogging[R] = new ZUniversalContextLogging[R](name, useContextValue)
}
