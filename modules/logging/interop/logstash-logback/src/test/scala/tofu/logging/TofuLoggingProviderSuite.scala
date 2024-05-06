package tofu.logging

import cats.Id
import cats.data.ReaderT
import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator}
import derevo.derive
import org.scalatest.funsuite.AnyFunSuite
import org.slf4j.LoggerFactory
import tofu.Delay

import java.io.StringWriter
import scala.jdk.CollectionConverters._

class TofuLoggingProviderSuite extends AnyFunSuite {
  import TofuLoggingProviderSuite._
  type F[T] = ReaderT[Id, LoggingContext, T]
  implicit val delay: Delay[F]       = new Delay[F] {
    override def delay[A](a: => A): F[A] = ReaderT[Id, LoggingContext, A] { _ => a }
  }
  implicit val logs: Logging.Make[F] = Logging.Make.contextual[F, LoggingContext]
  val appender                       = new ListAppender[ILoggingEvent]

  val logger = LoggerFactory.getLogger(this.getClass()).asInstanceOf[Logger]
  appender.start()
  logger.addAppender(appender)

  val defaultJsonFactory: JsonFactory = new JsonFactory()

  val provider: TofuLoggingProvider = new TofuLoggingProvider

  def buildLogJson(f: JsonGenerator => Unit): String = {
    val sw        = new StringWriter()
    val generator = defaultJsonFactory.createGenerator(sw)
    generator.writeStartObject()
    f(generator)
    generator.writeEndObject()
    generator.close()
    sw.close()
    sw.toString
  }

  val logging = logs.forService[TofuLoggingProviderSuite]
  test("serialize nested json structures") {
    logging.debug("Hello", Map("oh" -> "noo")).apply(ExampleCtx)
    val first = appender.list.asScala.head

    val log      = buildLogJson(provider.writeTo(_, first))
    val expected = """{"oh":"noo","inner":{"keks":2,"shreks":3},"what":"swamp"}"""

    assert(log === expected)
  }
}

object TofuLoggingProviderSuite {
  @derive(tofu.logging.derivation.loggable)
  final case class LoggingContext(inner: LoggingContextInner, what: String)

  @derive(tofu.logging.derivation.loggable)
  final case class LoggingContextInner(keks: Int, shreks: Int)

  val ExampleCtx = LoggingContext(LoggingContextInner(2, 3), "swamp")
}
