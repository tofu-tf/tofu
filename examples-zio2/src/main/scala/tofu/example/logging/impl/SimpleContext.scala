package tofu.example.logging.impl

import tofu.logging.{Loggable, LoggedValue}
import tofu.logging.zlogs.ContextProvider
import zio.{FiberRef, UIO, ULayer, ZLayer}

class SimpleContext(ref: FiberRef[String]) extends ContextProvider {
  private implicit val requestIdLoggable: Loggable[String] =
    Loggable.stringValue.named("requestId")

  override def getCtx: UIO[LoggedValue] = ref.get.map(x => x)

  def set(requestId: String): UIO[Unit] =
    ref.set(requestId)
}

object SimpleContext {
  val layer: ULayer[SimpleContext] = ZLayer.scoped(
    FiberRef
      .make[String]("")
      .map(new SimpleContext(_))
  )
}
