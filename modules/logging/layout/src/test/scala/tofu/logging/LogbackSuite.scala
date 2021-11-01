package tofu.logging

import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import org.scalatest.funsuite.AnyFunSuite
import org.slf4j.LoggerFactory
import tofu.{Delay, WithContext}

import scala.jdk.CollectionConverters._

class LogbackSuite extends AnyFunSuite {
  type Ctx = Map[String, String]
  implicit val delay: Delay[Ctx => *] = new Delay[Ctx => *] {
    def delay[A](a: => A) = _ => a
  }

  implicit val context: WithContext[Ctx => *, Ctx] = WithContext.make(identity)

  implicit val logs: Logging.Make[Ctx => *] = Logging.Make.contextual[Ctx => *, Ctx]

  val appender = new ListAppender[ILoggingEvent]

  val logger = LoggerFactory.getLogger(this.getClass()).asInstanceOf[Logger]
  println(logger.iteratorForAppenders.asScala.toList)
  appender.start()
  logger.addAppender(appender)

  val logging = logs.forService[LogbackSuite]
  test("throwable sent as throwable") {

    val error = new Exception("Wild Effect Appeared")

    logging.debugCause("Hello", error, Map("oh" -> "noo"))(Map("ehm" -> "umm"))

    Thread.sleep(100)

    val first = appender.list.asScala.head.getThrowableProxy()
    assert(first.getMessage === "Wild Effect Appeared")
    assert(first.getClassName === classOf[Exception].getName)
  }
}
