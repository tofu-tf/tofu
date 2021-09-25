package tofu
package logging

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.CoreConstants.{LINE_SEPARATOR => EOL}
import ch.qos.logback.core.LayoutBase
import tethys._
import tethys.commons.RawJson
import tethys.jackson._
import tofu.logging.ELKLayout.Arguments
import tofu.logging.logback.EventLoggable
import tofu.logging.json.JsonEntry
import tofu.logging.json.instances._

/** logging layout writing JSON receivable by logstash */
class ELKLayout extends LayoutBase[ILoggingEvent] {
  private var arguments: Arguments          = Arguments.Merge
  private var customFields: List[JsonEntry] = List.empty

  private[this] implicit var eventLoggable: Loggable[ILoggingEvent] = EventLoggable.merge

  def setArgumentsField(name: String): Unit = {
    arguments = Arguments.Collect(name)
    updateEventLoggable()
  }

  def setArguments(value: String): Unit = {
    arguments = Arguments.parse(value)
    updateEventLoggable()
  }

  /** Appends optional static fields which will be merged into you structured json output.
    *
    * Usage (inside <layout>): <customFields>{ "a": 1, "b": { "c": true, "d": "great" } }</customFields>
    *
    * @param json
    *   object of static fields with their values.
    */
  def setCustomFields(json: String)                   =
    json.jsonAs[List[JsonEntry]] match {
      case Left(value)  => throw new Exception("Invalid json format for 'customFields'. Check your json.", value)
      case Right(value) => customFields = value
    }

  private def updateEventLoggable(): Unit             =
    eventLoggable = arguments match {
      case Arguments.Merge         => EventLoggable.merge
      case Arguments.Group         => EventLoggable.group
      case Arguments.Collect(name) => EventLoggable.collect(name)
    }

  override def doLayout(event: ILoggingEvent): String =
    if (customFields.isEmpty) ELKLayout.builder(event)
    else ELKLayout.builderWithCustomFields(customFields)(event)
}

object ELKLayout {

  val MarkersField    = "markers"
  val TimeStampField  = "@timestamp"
  val LoggerNameField = "loggerName"
  val ThreadNameField = "threadName"
  val LevelField      = "level"
  val HostNameField   = "hostName"
  val MessageField    = "message"
  val MessageIdField  = "messageId"
  val ExceptionField  = "exception"
  val StackTraceField = "stackTrace"

  val builder: TethysBuilder = TethysBuilder(postfix = EOL)

  def builderWithCustomFields(customFields: List[(String, RawJson)]): TethysBuilder =
    TethysBuilder.withCustomFields(customFields = customFields, postfix = EOL)

  sealed trait Arguments

  object Arguments {
    case object Merge                 extends Arguments
    case object Group                 extends Arguments
    case class Collect(field: String) extends Arguments

    def parse(name: String): Arguments = name match {
      case "merge" => Merge
      case "group" => Group
    }
  }

  case class ArgumentParsingException(value: String) extends RuntimeException(s"could not parse $value")
}
