package tofu.logging

import ch.qos.logback.classic.spi.ILoggingEvent
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator}
import net.logstash.logback.composite.{AbstractJsonProvider, JsonFactoryAware}
import tethys.jackson.JacksonTokenWriter
import tethys.writers.tokens.TokenWriter
import tofu.logging.impl.ContextMarker

import scala.beans.BeanProperty

class TofuLoggingProvider extends AbstractJsonProvider[ILoggingEvent] with JsonFactoryAware {
  import TethysBuilder.receiver
  @BeanProperty
  protected var jsonFactory: JsonFactory = null

  override def writeTo(generator: JsonGenerator, event: ILoggingEvent): Unit = {
    val writer: TokenWriter = new JacksonTokenWriter(generator)
    Option(event.getArgumentArray).foreach {
      _.foreach {
        case lv: LoggedValue =>
          lv.logFields(writer)(receiver)
        case _               =>
      }
    }

    event.getMarker match {
      case ContextMarker(ctx, _) =>
        ctx.logFields(writer)(receiver)
      case _                     =>
    }
  }
}
