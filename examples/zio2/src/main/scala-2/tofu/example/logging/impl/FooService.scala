package tofu.example.logging.impl

import tofu.logging.zlogs.{ULogging, ZLogging}
import zio._

class FooService(
    logs: ZLogging.Make
) {
  private val logger: ULogging = logs.forService[FooService]

  def foo(i: Int): UIO[Int] =
    for {
      _ <- logger.debug("Starting Foo! i={}", i)
      _ <- Clock.sleep(i.seconds)
      _ <- logger.info("Foo completed!")
    } yield i
}

object FooService {
  val layer: URLayer[ZLogging.Make, FooService] = ZLayer.fromFunction(new FooService(_))
}
