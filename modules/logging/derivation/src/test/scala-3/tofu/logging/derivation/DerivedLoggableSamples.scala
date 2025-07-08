package tofu.logging.derivation

import java.time.LocalDate
import java.util.UUID
import tofu.logging.derivation.MaskMode.Custom
import tofu.logging.Loggable
import tofu.logging.SingletonEnumLoggable

object DerivedLoggableSamples:
  final case class Foo(lol: String, kek: Option[Long]) derives Loggable

  final case class Bar(
      @hidden foo1: Option[Foo] = None,
      @unembed foo2: Option[Foo] = None,
      foo3: Option[Foo] = None
  ) derives Loggable

  final case class Jak(
      @masked(MaskMode.Erase) one: String,
      @masked(MaskMode.ForLength(1)) two: Long,
      @masked(MaskMode.Regexp("\\d*\\.(\\d*)".r)) three: Double,
      @masked(MaskMode.Regexp("-?\\d*\\.(\\d*)".r)) four: List[Double],
  ) derives Loggable

  final case class Baz(foos: List[Foo] = Nil, ys: Vector[Int] = Vector(), zs: Option[List[List[String]]] = None)
      derives Loggable

  final case class MaskedBaz(@masked kek: Option[String], @ignoreOpt a: Option[String] = None) derives Loggable

  final case class MaskedOptBaz(
      @masked maybeStr: Option[String],
      @masked maybeInt: Option[Int],
      @masked maybeBool: Option[Boolean],
      @masked maybeDouble: Option[Double],
      @masked maybeStr2: Option[String]
  ) derives Loggable

  final case class MaskedCustom(
      @masked(Custom(_ => "*")) sensitiveField: String,
      @masked(Custom(name => name.take(1) + "***")) firstName: Option[String],
      @masked(Custom(i => "*" * i.length())) age: Int
  ) derives Loggable

  final case class MaskedContra(
      @masked(MaskMode.Erase) id: UUID,
      @masked(MaskMode.ForLength(4)) date: LocalDate,
  ) derives Loggable

  sealed trait SealedTraitEnum
  object SealedTraitEnum:
    case object A extends SealedTraitEnum
    case object B extends SealedTraitEnum
    case object C extends SealedTraitEnum
  given Loggable[SealedTraitEnum] = SingletonEnumLoggable.derived

  enum Scala3Enum:
    case A, B, C
  given Loggable[Scala3Enum] = SingletonEnumLoggable.derived

  final case class Container[A, B, C](a: A, b: B, c: C) derives Loggable
