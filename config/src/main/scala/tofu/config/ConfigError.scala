package tofu
package config

import cats.data.NonEmptyList
import tofu.config.ConfigItem.ValueTag
import cats.Show
import cats.syntax.show._
import cats.syntax.foldable._
import cats.instances.string._
import cats.instances.vector._

sealed trait ConfigError extends Product with Serializable
object ConfigError {
  case object NotFound                                                 extends ConfigError
  case object NoVariantFound                                           extends ConfigError
  final case class BadType(expected: List[ValueTag], actual: ValueTag) extends ConfigError
  final case class BadNumber(value: BigDecimal, message: String)       extends ConfigError
  final case class BadString(value: String, message: String)           extends ConfigError
  final case class Invalid(message: String)                            extends ConfigError
  final case class MultipleVariants(variants: NonEmptyList[String])    extends ConfigError

  implicit val configErrorShow: Show[ConfigError] = {
    case NotFound                   => "not found"
    case NoVariantFound             => "no variant found"
    case BadType(expected, actual)  => s"bad type, expected: ${expected.mkString(",")}, actual : $actual"
    case BadNumber(value, message)  => s"bad number $value : $message"
    case BadString(value, message)  => s"bad string '$value' : $message"
    case MultipleVariants(variants) => s"multiple variants found : $variants"
    case Invalid(message)           => message
  }
}

final case class ConfigParseMessage(path: Path, error: ConfigError)

object ConfigParseMessage{
  implicit val messageShow: Show[ConfigParseMessage] = message => {
    val pathShown = message.path.iterator.map(_.show).mkString(".")  
    show"$pathShown : ${message.error}"
  }
}

final case class ConfigParseErrors(ms: MessageList) extends RuntimeException(ms.mkString_("\n"))


