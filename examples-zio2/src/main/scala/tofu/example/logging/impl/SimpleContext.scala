package tofu.example.logging.impl

import tofu.logging.Loggable
import tofu.logging.zlogs.ValueContextProvider
import zio.{FiberRef, UIO, ULayer, ZLayer}

class SimpleContext(ref: FiberRef[String])
    extends ValueContextProvider[String]()(Loggable.stringValue.named("requestId")) {

  override def getA: UIO[String] = ref.get

  def set(requestId: String): UIO[Unit] =
    ref.set(requestId)
}

object SimpleContext {
  val layer: ULayer[SimpleContext] = ZLayer.scoped(
    FiberRef
      .make[String]("undefined_request_id")
      .map(new SimpleContext(_))
  )
}
