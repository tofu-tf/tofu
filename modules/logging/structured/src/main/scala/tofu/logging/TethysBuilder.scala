package tofu
package logging

import java.io.StringWriter

import cats.instances.unit._
import cats.kernel.Monoid
import tofu.logging.LogRenderer.LogRendererUnit
import tethys.commons.RawJson
import tethys.writers.tokens.TokenWriter

class TethysBuilder(prefix: String = "", postfix: String = "") extends LogBuilder[String] {
  type Top    = TokenWriter
  type Value  = TokenWriter
  type ValRes = Boolean
  type Output = Unit

  /** override to add predefined fields */
  def predefined(tokenWriter: TokenWriter): Unit = {}

  def writeValue(value: LogParamValue, writer: TokenWriter): Unit = value match {
    case StrValue(v)     => writer.writeString(v)
    case IntValue(v)     => writer.writeNumber(v)
    case BigIntValue(v)  => writer.writeNumber(v)
    case DecimalValue(v) => writer.writeNumber(v)
    case FloatValue(v)   => writer.writeNumber(v)
    case BoolValue(v)    => writer.writeBoolean(v)
    case NullValue       => writer.writeNull()
  }

  def checkWritten(res: Boolean, writer: TokenWriter): Unit =
    if (!res) writer.writeNull()

  val receiver = new LogRendererUnit[Top, Value, ValRes] {

    def zero(v: TokenWriter): Boolean                                                           = false
    def coalesce(f: TokenWriter => Boolean, g: TokenWriter => Boolean, v: TokenWriter): Boolean = f(v) || g(v)

    def putValue(value: LogParamValue, writer: TokenWriter): Boolean                          = {
      writeValue(value, writer)
      true
    }
    def sub(name: String, writer: TokenWriter)(receive: TokenWriter => Boolean): Unit         = {
      writer.writeFieldName(name)
      checkWritten(receive(writer), writer)
    }
    def list(size: Int, writer: TokenWriter)(receive: (TokenWriter, Int) => Boolean): Boolean = {
      writer.writeArrayStart()
      for (i <- 0 until size) receive(writer, i)
      writer.writeArrayEnd()
      true
    }
    def dict(writer: TokenWriter)(receive: TokenWriter => Unit): Boolean                      = {
      writer.writeObjectStart()
      receive(writer)
      writer.writeObjectEnd()
      true
    }

    // overriden just to drive-out one more allocation
    override def addField(name: String, value: LogParamValue, writer: TokenWriter): Unit = {
      writer.writeFieldName(name)
      writeValue(value, writer)
    }

    // optimized set of non-wrapping field writers
    override def addString(name: String, value: String, input: TokenWriter): Unit      = {
      input.writeFieldName(name)
      input.writeString(value)
    }
    override def addInt(name: String, value: Long, input: TokenWriter): Unit           = {
      input.writeFieldName(name)
      input.writeNumber(value)
    }
    override def addFloat(name: String, value: Double, input: TokenWriter): Unit       = {
      input.writeFieldName(name)
      input.writeNumber(value)
    }
    override def addBigInt(name: String, value: BigInt, input: TokenWriter): Unit      = {
      input.writeFieldName(name)
      input.writeNumber(value)
    }
    override def addDecimal(name: String, value: BigDecimal, input: TokenWriter): Unit = {
      input.writeFieldName(name)
      input.writeNumber(value)
    }
    override def addBool(name: String, value: Boolean, input: TokenWriter): Unit       = {
      input.writeFieldName(name)
      input.writeBoolean(value)
    }
    override def putString(value: String, input: TokenWriter): Boolean                 = { input.writeString(value); true }
    override def putInt(value: Long, input: TokenWriter): Boolean                      = { input.writeNumber(value); true }
    override def putFloat(value: Double, input: TokenWriter): Boolean                  = { input.writeNumber(value); true }
    override def putBigInt(value: BigInt, input: TokenWriter): Boolean                 = { input.writeNumber(value); true }
    override def putDecimal(value: BigDecimal, input: TokenWriter): Boolean            = { input.writeNumber(value); true }
    override def putBool(value: Boolean, input: TokenWriter): Boolean                  = { input.writeBoolean(value); true }
  }

  def monoid: Monoid[Unit] = implicitly

  def make(f: TokenWriter => Unit): String = {
    val sw     = new StringWriter()
    sw.append(prefix)
    val writer = tethys.jackson.jacksonTokenWriterProducer.forWriter(sw)
    writer.writeObjectStart()
    predefined(writer)
    f(writer)
    writer.writeObjectEnd()
    writer.flush()
    sw.append(postfix)
    sw.toString
  }
}

class TethysBuilderWithCustomFields(customFields: List[(String, RawJson)], prefix: String = "", postfix: String = "")
    extends TethysBuilder(prefix, postfix) {

  override def predefined(tokenWriter: TokenWriter): Unit = {
    customFields.foreach { case (key, json) =>
      tokenWriter.writeFieldName(key)
      tokenWriter.writeRawJson(json.json)
    }
  }
}

object TethysBuilder extends TethysBuilder("", "") {
  def apply(prefix: String = "", postfix: String = "") = new TethysBuilder(prefix, postfix)

  def withCustomFields(customFields: List[(String, RawJson)], prefix: String = "", postfix: String = "") =
    new TethysBuilderWithCustomFields(customFields, prefix, postfix)
}
