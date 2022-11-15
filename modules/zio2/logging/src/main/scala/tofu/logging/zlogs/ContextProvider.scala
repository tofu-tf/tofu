package tofu.logging.zlogs

import tofu.logging.LoggedValue
import zio.UIO

trait ContextProvider {
  def getCtx: UIO[LoggedValue]
}
