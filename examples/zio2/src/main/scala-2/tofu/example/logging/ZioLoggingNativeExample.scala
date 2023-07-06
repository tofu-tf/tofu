package tofu.example.logging

import tofu.logging.zlogs.ZLogAnnotation.loggerName
import tofu.logging.zlogs._
import zio.{LogAnnotation => _, _}

object ZioLoggingNativeExample extends ZIOAppDefault {

  def zioTask(i: Int) =
    ZIO.logSpan(s"backend_$i") { // measure how much time the current job spent
      val zio = for {
        requestId <- Random.nextUUID
        _         <- {
                       for {
                         _ <- ZIO.log("Start request")
                         // {"zSpans":{"backend_2":10,"full_app":35}}
                         _ <- ZIO.sleep(i.seconds)
                         // every log in scope will contain: "user":{"login":"Vasya","accessLevel":1}
                         _ <- LogKey.user.scoped(TestUser("Vasya", "12345"))
                         _ <- ZIO.fail(new IllegalStateException(s"$i divided by 3")).when(i % 3 == 0)
                         _ <- ZIO.logInfo("Request completed") // + i seconds:
                         // {"zSpans":{"backend_2":2032,"full_app":2063}}
                       } yield ()
                     } @@
                       // every log above will contain: {"count":1}
                       LogKey.count(i) @@
                       // ... and specified {"loggerName":"MyLogger"}
                       loggerName("MyLogger") @@
                       // you can also provide optional values:
                       LogKey.requestId(Some(requestId))
      } yield ()

      zio.catchAllCause(c => ZIO.logErrorCause("Request failed", c))
    }

  val tasks = (1 to 3).map(zioTask)

  override def run = {
    ZIO.logInfo("Before starting") *> // only message without context
      // if not specified, loggerName is parsed from auto generated Trace:
      // {"loggerName":"tofu.example.logging.ZioLoggingNativeExample"}
      ZIO.logSpan("full_app") {
        // logs will contain {"zSpans":{"full_app":6}}: this is
        // the time (milliseconds) spent from the span started
        ZIO.scoped {
          for {
            // adds object with zioAnnotations: {"zSpans":{"full_app":6},"zAnnotations":{"detail":"Good day!"}}
            _ <- ZIO.logWarning("Hello everybody") @@ ZIOAspect.annotated("detail" -> "Good day!")
            // adds tofu-anotation for every parallel task:
            // {"user":{"login":"Vasya","accessLevel":1}}
            _ <- ZIO.collectAllParDiscard(tasks) @@ LogKey.user(TestUser("Vasya", "12345"))
            // {"message":"Application shut down","zSpans":{"full_app":3064}}
            _ <- ZIO.log("Application shut down")
          } yield ()
        }
      }
  }
    .provide(
      Runtime.removeDefaultLoggers >>> TofuZLogger.addToRuntime,
    )
}
