package tofu
package config

import cats.data.NonEmptyList
import tofu.config.ConfigItem.ValueTag

sealed trait ConfigError extends Throwable with Product with Serializable
object ConfigError {
  case object NotFound                                                 extends ConfigError
  case object NoVariantFound                                           extends ConfigError
  final case class BadType(expected: List[ValueTag], actual: ValueTag) extends ConfigError
  final case class BadNumber(value: BigDecimal, message: String)       extends ConfigError
  final case class BadString(value: String, message: String)           extends ConfigError
  final case class Invalid(message: String)                            extends ConfigError
  final case class MultipleVariants(variants: NonEmptyList[String])    extends ConfigError
}
