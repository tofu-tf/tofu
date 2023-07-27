package tofu.logging.zlogs

import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import tofu.logging.zlogs.TestStuff.User
import tofu.logging.{LogTree, LoggedValue}
import zio._
import zio.test._

object TofuDefaultContextSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("TofuDefaultContext")(
      test("extracts all values added via ZLogAnnotation") {
        {
          for {
            ctx    <- ZIO.service[TofuDefaultContext]
            status <- ctx.getValue(LogKey.status)
            user   <- ctx.getValue(LogKey.user)
            count  <- ctx.getValue(LogKey.count)
          } yield {
            assertTrue(status.get == testStatus) &&
            assertTrue(user.get == testUser) &&
            assertTrue(count.get == testCount)
          }
        } @@ LogKey.user(testUser) @@ LogKey.count(testCount) @@ LogKey.status(testStatus)
      }.provide(TofuDefaultContext.layerZioContextOff),
      test("extracts all values added via ZLogAnnotation.scoped") {
        ZIO.scoped {
          for {
            _      <- scopedTestProg
            ctx    <- ZIO.service[TofuDefaultContext]
            status <- ctx.getValue(LogKey.status)
            user   <- ctx.getValue(LogKey.user)
            count  <- ctx.getValue(LogKey.count)
          } yield {
            assertTrue(status.get == testStatus) &&
            assertTrue(user.get == testUser) &&
            assertTrue(count.get == testCount)
          }
        }
      }.provide(TofuDefaultContext.layerZioContextOff),
      test("layerZioContextOffLayer logs only tofu context") {
        testProg.map { ctx =>
          assertTrue(LogTree(ctx) == expectedJsonOnlyTofuContext)
        }.provideLayer(TofuDefaultContext.layerZioContextOff)
      },
      test("layerZioContextOn logs only zio context too") {
        testProg.map { ctx =>
          assertTrue(LogTree(ctx) == expectedJsonWithZIOContext)
        }.provideLayer(TofuDefaultContext.layerZioContextOn)
      }
    )

  val testStatus     = "OK"
  val testCount      = 100500
  val testUser: User = User("John")

  val adjustDuration: Duration = 5.seconds

  val testProg: URIO[ContextProvider, LoggedValue] = TestStuff.addLogSpan("testSpan")(
    TestClock.adjust(adjustDuration) *>
      ZIO.serviceWithZIO[ContextProvider](_.getCtx)
  ) @@ ZIOAspect.annotated("foo", "bar") @@ LogKey.status(testStatus) @@ LogKey.count(testCount)

  val scopedTestProg: ZIO[Scope, Nothing, Unit] =
    for {
      _ <- LogKey.user.scoped(testUser)
      _ <- LogKey.count.scoped(testCount)
      _ <- LogKey.status.scoped(Some(testStatus))
      _ <- TestClock.adjust(adjustDuration)
    } yield ()

  val expectedJsonOnlyTofuContext: Json = JsonObject(
    LogKey.count.name  -> Json.fromInt(testCount),
    LogKey.status.name -> testStatus.asJson
  ).asJson

  val justZIOContextJson: Json = JsonObject(
    "zSpans"       -> JsonObject(
      "testSpan" -> Json.fromLong(adjustDuration.toMillis)
    ).asJson,
    "zAnnotations" -> JsonObject(
      "foo" -> "bar".asJson
    ).asJson
  ).asJson

  val expectedJsonWithZIOContext: Json = justZIOContextJson.deepMerge(expectedJsonOnlyTofuContext)
}
