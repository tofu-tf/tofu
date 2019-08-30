package tofu
package config

import cats.Monad
import cats.data.NonEmptyList
import tofu.config.ConfigItem.ValueTag

trait ConfigErrors[F[_]] extends ConfigErrors.NotFound[F] with ConfigErrors.Variants[F] with ConfigErrors.Value[F]

object ConfigErrors {
  def notFound[F[_], A](implicit nf: NotFound[F]): F[A] = nf.notFound
  def badType[F[_], A](expected: ValueTag*)(actual: ValueTag)(implicit error: Value[F]): F[A] =
    error.badType(expected.toList, actual)

  trait NotFound[F[_]] {
    def notFound[A]: F[A]
  }

  trait Variants[F[_]] {
    def multipleVariants[A](variants: NonEmptyList[String]): F[A]
    def noVariantFound[A]: F[A]
  }

  trait Value[F[_]] {
    def badType[A](expected: List[ValueTag], actual: ValueTag): F[A]
    def badString[A](value: String, message: String): F[A]
    def badNumber[A](value: BigDecimal, message: String): F[A]
    def invalid[A](message: String): F[A]
  }

  sealed trait Message
  object Message {
    case object NotFound                                                 extends Message
    case object NoVariantFound                                           extends Message
    final case class BadType(expected: List[ValueTag], actual: ValueTag) extends Message
    final case class BadNumber(value: BigDecimal, message: String)       extends Message
    final case class BadString(value: String, message: String)           extends Message
    final case class Invalid(message: String)                            extends Message
    final case class MultipleVariants(variants: NonEmptyList[String])    extends Message
  }

  def byError[F[_]: Monad](implicit error: F Raise Message): ConfigErrors[F] =
    new ConfigErrors[F] {
      def notFound[A]: F[A]                                            = error.raise(Message.NotFound)
      def badType[A](expected: List[ValueTag], actual: ValueTag): F[A] = error.raise(Message.BadType(expected, actual))
      def badString[A](value: String, message: String): F[A]           = error.raise(Message.BadString(value, message))
      def badNumber[A](value: BigDecimal, message: String): F[A]       = error.raise(Message.BadNumber(value, message))
      def invalid[A](message: String): F[A]                            = error.raise(Message.Invalid(message))
      def multipleVariants[A](variants: NonEmptyList[String]): F[A]    = error.raise(Message.MultipleVariants(variants))
      def noVariantFound[A]: F[A]                                      = error.raise(Message.NoVariantFound)
    }
}