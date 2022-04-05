package tofu.logging

import ch.qos.logback.classic.spi.ILoggingEvent
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator}
import net.logstash.logback.composite.{AbstractJsonProvider, JsonFactoryAware, JsonWritingUtils}
import tofu.logging.impl.ContextMarker

import scala.beans.BeanProperty

class TofuLoggingProvider extends AbstractJsonProvider[ILoggingEvent] with JsonFactoryAware {
  @BeanProperty
  protected var jsonFactory: JsonFactory = null

  override def writeTo(generator: JsonGenerator, event: ILoggingEvent): Unit = {
    Option(event.getArgumentArray).foreach {
      _.foreach {
        case lv: LoggedValue =>
          lv.foreachLog { case (name, value) =>
            JsonWritingUtils.writeStringField(generator, name, value.toString)
          }
        case _               =>
      }
    }

    event.getMarker match {
      case ContextMarker(ctx, _) =>
        ctx.foreachLog { (name, value) =>
          JsonWritingUtils.writeStringField(generator, name, value.toString)
        }
      case _                     =>
    }
  }
}
