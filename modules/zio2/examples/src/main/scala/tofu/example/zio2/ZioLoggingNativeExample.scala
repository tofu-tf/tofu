package tofu.example.zio2

import tofu.logging.zlogs._
import zio._

object ZioLoggingNativeExample extends ZIOAppDefault {

  def zioTask(i: Int) = ZIO.logSpan(s"backend_$i") {
    val zio = for {
      requestId <- Random.nextUUID
      _         <- {
        for {
          _ <- ZIO.log("Start request")
          _ <- ZIO.sleep(i.seconds)
          _ <- ZIO.fail(new IllegalStateException(s"$i divided by 5")).when(i % 5 == 0)
          _ <- ZIO.logInfo("Request completed")
        } yield ()
      }
    } yield ()

    zio.catchAllCause(c => ZIO.logErrorCause("Request failed", c))
  }

  val tasks = (1 to 3).map(zioTask)

  override def run = {
    ZIO.logInfo("Before starting") *>
      ZIO.logSpan("full_app") {
        for {
          _  <- ZIO.logWarning("Hello everybody") @@ ZIOAspect.annotated("detail" -> "Good day!")
          _  <- ZIO.collectAllParDiscard(tasks) @@ ZLogAspect(LogKey.user -> TestUser("Vasya", "12345"))()
          _  <- ZIO.log("Application shut down")
        } yield ()
      }
  }.provide(
    Runtime.removeDefaultLoggers >>> TofuZLogger.addToRuntime,
  )
}
