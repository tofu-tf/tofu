package tofu.example.zio2

import tofu.logging.zlogs._
import zio.{LogAnnotation => _, _}

object Test extends ZIOAppDefault {

  def zioTask(i: Int) = ZIO.logSpan(s"backend_$i") {
    val zio = for {
      lc        <- ZIO.service[ZLogContext]
      requestId <- Random.nextUUID
      _         <- lc.update(LogKey.requestId -> requestId)
      _         <- lc.update(LogKey.count -> i)
      _         <- ZIO.log("Start request")
      _         <- ZIO.sleep(i.seconds)
      _         <- ZIO.fail(new IllegalStateException(s"$i divided by 5")).when(i % 5 == 0)
      _         <- ZIO.logInfo("Request completed")
    } yield ()

    zio.catchAllCause(c => ZIO.logErrorCause("Request failed", c))
  }

  val tasks = (1 to 9).map(zioTask)

  override def run = {
    ZIO.logInfo("Before starting") *>
      ZIO.logSpan("full_app") {
        for {
          lc <- ZIO.service[ZLogContext]
          _  <- ZIO.logWarning("Hello everybody") @@ ZIOAspect.annotated("detail" -> "Good day!")
          _  <- lc.update(LogKey.user -> TestUser("Vasya", "12345"))
          _  <- ZIO.collectAllParDiscard(tasks)
          _  <- ZIO.log("Application shut down")
        } yield ()
      }
  }.provide(
    Runtime.removeDefaultLoggers >>> TofuZLogger.addToRuntime,
    ZLogContextLive.layer
  )
}
