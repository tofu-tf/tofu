package tofu.logging.zlogs

import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import derevo.derive
import io.circe.JsonObject
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory
import tofu.logging.LogTree
import tofu.logging.derivation.loggable
import tofu.logging.impl.ContextMarker
import tofu.syntax.logging._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.{Has, Runtime, URIO, URLayer, ZLayer}

import scala.jdk.CollectionConverters._

class ZLogsSuite extends AnyFlatSpec with Matchers {
  import ZLogsSuite.MyLogging

  val expr = debug"hello" *> info"world"

  "ZLogs" should "log the context" in {
    val appender = ZLogsSuite.attachList()
    Runtime.default.unsafeRun(expr.provideLayer(ZLogsSuite.fullLayer))
    val items    = appender.list.asScala

    val expected = JsonObject("foo" -> "kojima".asJson, "bar" -> 2.asJson).asJson

    items.map(_.getMarker).collect { case ContextMarker(ctx, _) =>
      LogTree(ctx)
    } should ===(List.fill(2)(expected))
  }
}

object ZLogsSuite {
  val Name = "zio logs suite"

  @derive(loggable)
  case class FooService(foo: String)

  val fooLayer = ZLayer.succeed(FooService("kojima"))

  @derive(loggable)
  case class BarService(bar: Int)

  val barLayer = ZLayer.succeed(BarService(2))

  type Foo = Has[FooService]
  type Bar = Has[BarService]

  type LogEnv    = Foo with Bar
  type SystemEnv = Blocking with Clock with Console
  type MyEnv     = SystemEnv with LogEnv with ZLog[LogEnv]
  type TIO[+A]   = URIO[MyEnv, A]

  val logs: ZLogs[Foo with Bar] = ZLogs.build.of[Foo].of[Bar].make

  implicit val MyLogging: ZLogging[MyEnv] = ZLogs.access[MyEnv, LogEnv]

  implicitly[ZioHasBuilder.UnHas[Foo]](ZioHasBuilder.UnHas.unHas[FooService])
  val fullLayer: URLayer[Blocking with Console with Clock, MyEnv] =
    ZLayer.identity[SystemEnv] ++ fooLayer ++ barLayer ++ ZLogs.named(logs, Name)

  def attachList() = {
    val logger   = LoggerFactory.getLogger(Name).asInstanceOf[Logger]
    val appender = new ListAppender[ILoggingEvent]
    appender.start()
    logger.addAppender(appender)
    appender
  }
}
