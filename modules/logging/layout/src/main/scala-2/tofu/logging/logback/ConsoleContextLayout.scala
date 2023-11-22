package tofu.logging.logback

import java.util

import ch.qos.logback.classic.{Level, PatternLayout}
import ch.qos.logback.classic.spi.{ILoggingEvent, IThrowableProxy, LoggerContextVO}
import org.slf4j.Marker
import org.slf4j.event.KeyValuePair
import tofu.logging.LoggedValue
import tofu.logging.impl.ContextMarker

class ConsoleContextLayout extends PatternLayout {

  override def doLayout(event: ILoggingEvent): String =
    super.doLayout(new WrappedEvent(event))
}

class WrappedEvent(event: ILoggingEvent) extends ILoggingEvent {

  lazy val getMDCPropertyMap: util.Map[String, String] = {
    val map                   = new util.HashMap[String, String]
    map.putAll(event.getMDCPropertyMap)
    def intoMdc(x: Any): Unit = x match {
      case lv: LoggedValue =>
        lv.foreachLog { (name, value) =>
          map.put(name, value.toString)
          ()
        }
      case _               =>
    }

    event.getMarker match {
      case ContextMarker(ctx, _) => intoMdc(ctx)
      case _                     =>
    }

    for (arr <- Option(event.getArgumentArray); arg <- arr) intoMdc(arg)

    map
  }

  def getThreadName: String                     = event.getThreadName
  def getLevel: Level                           = event.getLevel
  def getMessage: String                        = event.getMessage
  def getArgumentArray: Array[AnyRef]           = event.getArgumentArray
  def getFormattedMessage: String               = event.getFormattedMessage
  def getLoggerName: String                     = event.getLoggerName
  def getLoggerContextVO: LoggerContextVO       = event.getLoggerContextVO
  def getThrowableProxy: IThrowableProxy        = event.getThrowableProxy
  def getCallerData: Array[StackTraceElement]   = event.getCallerData
  def hasCallerData: Boolean                    = event.hasCallerData
  def getMarkerList: util.List[Marker]          = event.getMarkerList
  def getMdc: util.Map[String, String]          = getMDCPropertyMap
  def getTimeStamp: Long                        = event.getTimeStamp
  def getNanoseconds: Int                       = event.getNanoseconds
  def getSequenceNumber: Long                   = event.getSequenceNumber
  def getKeyValuePairs: util.List[KeyValuePair] = event.getKeyValuePairs
  def prepareForDeferredProcessing(): Unit      = event.prepareForDeferredProcessing()
}
