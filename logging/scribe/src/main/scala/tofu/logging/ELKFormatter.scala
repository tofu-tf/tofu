package tofu.logging

import _root_.scribe.LogRecord
import _root_.scribe.output.{LogOutput, TextOutput}
import _root_.scribe.format.Formatter

import tofu.logging.ELKFormatter.builder
import tofu.logging.scribe.RecordLoggable

final case class ELKFormatter(loggable: Loggable[RecordLoggable.LogRecord]) extends Formatter {
  def format[M](rec: LogRecord[M]): LogOutput =
    new TextOutput(builder(rec)(loggable.asInstanceOf[Loggable[LogRecord[M]]]))
}

object ELKFormatter {
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

  val builder: TethysBuilder = TethysBuilder()

  def merge: ELKFormatter = ELKFormatter(RecordLoggable.merge)
}