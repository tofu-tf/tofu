package tofu.logging.scribe.internal

import org.slf4j.Marker

import scribe.{LogRecord, Loggable}
import scribe.output.LogOutput

final class MarkedOutput(val marker: Marker, val plainText: String, val args: Array[AnyRef])
  extends LogOutput with Product3[Marker, String, Array[AnyRef]] {

  def map(f: String => String): LogOutput = MarkedOutput(marker, f(plainText), args)

  def _1: Marker = marker
  def _2: String = plainText
  def _3: Array[AnyRef] = args

  def canEqual(that: Any): Boolean = that match {
    case _: MarkedOutput => true
    case _               => false
  }
}

object MarkedOutput {
  final type Type = Product3[Marker, String, Array[AnyRef]]

  final def apply(marker: Marker, plainText: String, args: Array[AnyRef]) = new MarkedOutput(marker, plainText, args)

  final def apply(product: Type): MarkedOutput = product match {
    case out: MarkedOutput => out
    case t                 => apply(t._1, t._2, t._3)
  }

  final def unapply(rec: LogRecord[Any]): Option[Type] = rec.loggable match {
    case lg: NonBoxingLoggable.type => Some(lg(rec.message.value).asInstanceOf[MarkedOutput])
    case _                          => None
  }

  object NonBoxingLoggable extends Loggable[Type] {
    def apply(value: Type): LogOutput = MarkedOutput(value)
  }
}