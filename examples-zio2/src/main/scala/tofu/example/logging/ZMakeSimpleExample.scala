package tofu.example.logging

import tofu.example.logging.impl.{FooService, SimpleContext}
import tofu.logging.zlogs.ZLogging
import zio._

object ZMakeSimpleExample extends ZIOAppDefault {

  val tasks = (1 to 3)
    .map(i =>
      for {
        id <- Random.nextIntBetween(1000, 4000)
        _  <- ZIO.serviceWithZIO[SimpleContext](_.set(id.toHexString))
        _  <- ZIO.serviceWithZIO[FooService](_.foo(i))
      } yield ()
    )

  override def run =
    ZIO
      .collectAllParDiscard(tasks)
      .provide(
        FooService.layer,
        ZLogging.Make.layerPlainWithContext,
        SimpleContext.layer
      )
}
