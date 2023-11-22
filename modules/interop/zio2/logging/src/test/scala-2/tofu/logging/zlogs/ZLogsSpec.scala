package tofu.logging.zlogs

import ch.qos.logback.classic.Level
import io.circe.{Json, JsonObject}
import tofu.logging.impl.ContextMarker
import tofu.logging.zlogs.TestStuff._
import tofu.logging.zlogs.TofuDefaultContextSpec.{testCount, testStatus, testUser}
import tofu.logging.{LogTree, LoggedValue}
import zio._
import zio.test._

case object ZLogsSpec extends ZIOSpecDefault {

  val loggerName: String = this.getClass.getName.replace("$", "") // tofu.logging.zlogs.ZLogsSpec

  println(loggerName)

  val myLoggerName = "myLogger"

  val expectedContextValue: LoggedValue = LogKey.user -> testUser

  val contextProvider: ULayer[ContextProvider] = ZLayer.succeed(new ContextProvider {
    override def getCtx: UIO[LoggedValue] = ZIO.succeed(expectedContextValue)
  })

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Tofu ZIO2 Logging")(
      test("TofuZLogger parses default logger name") {
        val expectedArgs: Set[Json] =
          Set[LoggedValue](LogKey.status -> testStatus, LogKey.count -> testCount).map(LogTree(_))
        addLogSpan("testSpan")(
          LogLevel.Warning {
            for {
              _      <- TestClock.adjust(5.seconds)
              _      <- ZIO.log("Some message")
              events <- LogAppender.events
            } yield {
              val e   = events.head
              val ctx = e.getMarker.asInstanceOf[ContextMarker].ctx
              assertTrue(LogTree(ctx) == TofuDefaultContextSpec.justZIOContextJson) &&
              assertTrue(
                e.getArgumentArray.toSet
                  .asInstanceOf[Set[LoggedValue]]
                  .map(LogTree(_)) == expectedArgs
              ) &&
              assertTrue(e.getLevel == Level.WARN)
            }
          }
        ) @@ ZIOAspect.annotated("foo", "bar") @@ LogKey.status(testStatus) @@ LogKey.count(
          testCount
        )
      }.provide(
        Runtime.removeDefaultLoggers,
        TofuZLogger.addToRuntime,
        LogAppender.layer(loggerName)
      ),
      test("TofuZLogger uses user-defined logger name") {
        (
          for {
            _      <- ZIO.log("Some message")
            events <- LogAppender.events
          } yield {
            val e    = events.head
            val ctx  = e.getMarker.asInstanceOf[ContextMarker].ctx
            val args = e.getArgumentArray.toSet.asInstanceOf[Set[LoggedValue]]
            assertTrue(LogTree(ctx).asObject.get == JsonObject.empty) &&
            assertTrue(args.size == 1) && // non-printable loggerName
            assertTrue(LogTree(args.head).asObject.get == JsonObject.empty) &&
            assertTrue(e.getLevel == Level.INFO)
          }
        ) @@ ZLogAnnotation.loggerName(myLoggerName)
      }.provide(
        Runtime.removeDefaultLoggers,
        TofuZLogger.addToRuntime,
        LogAppender.layer(myLoggerName)
      ),
      test("ZLogs") {
        val logArg: LoggedValue = LogKey.count -> testCount
        for {
          _      <- ZIO.serviceWithZIO[ULogging](_.info("Some message", logArg))
          events <- LogAppender.events
        } yield {
          val e   = events.head
          val ctx = e.getMarker.asInstanceOf[ContextMarker].ctx
          assertTrue(e.getArgumentArray()(0).asInstanceOf[LoggedValue] == logArg) &&
          assertTrue(LogTree(ctx) == LogTree(expectedContextValue)) &&
          assertTrue(e.getLevel == Level.INFO)
        }
      }.provide(
        ZLogs.layerPlainWithContext,
        contextProvider,
        ZLayer(ZIO.serviceWithZIO[ULogs](_.byName(loggerName))),
        LogAppender.layer(loggerName)
      ),
      test("ZLogging.Make") {
        val logArg1: LoggedValue = LogKey.status -> testStatus
        val logArg2: LoggedValue = LogKey.user   -> testUser
        for {
          _      <- ZIO.serviceWithZIO[ULogging](_.error("Some message", logArg1, logArg2))
          events <- LogAppender.events
        } yield {
          val e   = events.head
          val ctx = e.getMarker.asInstanceOf[ContextMarker].ctx
          assertTrue(e.getArgumentArray()(0).asInstanceOf[LoggedValue] == logArg1) &&
          assertTrue(e.getArgumentArray()(1).asInstanceOf[LoggedValue] == logArg2) &&
          assertTrue(LogTree(ctx).asObject.get == JsonObject.empty) &&
          assertTrue(e.getLevel == Level.ERROR)
        }
      }.provide(
        ZLogging.Make.layerPlain,
        ZLayer(ZIO.serviceWith[ZLogging.Make](_.byName(loggerName))),
        LogAppender.layer(loggerName)
      )
    ) @@ TestAspect.sequential

}
