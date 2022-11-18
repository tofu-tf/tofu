package tofu.logging.zlogs

import tofu.logging.{Loggable, LoggedValue}
import zio.UIO

trait ContextProvider {
  def getCtx: UIO[LoggedValue]
}

abstract class ValueContextProvider[A](implicit L: Loggable[A]) extends ContextProvider {
  protected def getA: UIO[A]

  override def getCtx: UIO[LoggedValue] = getA.map(x => x)
}
