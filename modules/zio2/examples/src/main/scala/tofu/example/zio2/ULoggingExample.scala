package tofu.example.zio2
import tofu.logging.zlogs._
import zio._

object ULoggingExample extends ZIOAppDefault {

  def run = {
    for {
      service <- ZIO.service[MyService]
      _       <- ZIO.serviceWithZIO[ULogging](_.info("Start application"))
      lc      <- ZIO.service[ZLogContext]
      tasks    = (1 to 9)
                   .map(i => lc.locally(_.update(LogKey.count, i))(service.doSmth(i)))
      _       <- ZIO.collectAllParDiscard(tasks)
    } yield ()
  }.provide(
    MyService.layer,
    ZLogContext.live,
    ZLogs.layerPlainWithContext,
    ZLayer(ZIO.serviceWithZIO[ULogs](_.forService[MyService]))
  )

}

class MyService(
    logger: ULogging,
    logContext: ZLogContext
) {

  def user(i: Int): TestUser = TestUser("Vasya", "1234", i)

  def doSmth(i: Int): UIO[Unit] =
    for {
      _  <- logger.warn("Starting request", LogKey.user -> user(i))
      _  <- ZIO.sleep(i.seconds)
      id <- Random.nextUUID
      _  <- logContext.update(LogKey.requestId, id)
      _  <- logger.info("Request completed")
    } yield ()
}

object MyService {
  val layer: RLayer[ULogging & ZLogContext, MyService] =
    ZLayer.fromFunction(new MyService(_, _))
}
