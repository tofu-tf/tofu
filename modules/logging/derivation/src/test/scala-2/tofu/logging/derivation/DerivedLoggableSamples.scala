package tofu.logging.derivation

import java.time.LocalDate
import java.util.UUID

import derevo.derive
import tofu.logging.derivation.MaskMode.Custom

object DerivedLoggableSamples {
  @derive(loggable)
  final case class Foo(lol: String, kek: Option[Long])

  @derive(loggable)
  final case class Bar(
      @hidden foo1: Option[Foo] = None,
      @unembed foo2: Option[Foo] = None,
      foo3: Option[Foo] = None
  )

  @derive(loggable)
  final case class Jak(
      @masked(MaskMode.Erase) one: String,
      @masked(MaskMode.ForLength(1)) two: Long,
      @masked(MaskMode.Regexp("\\d*\\.(\\d*)".r)) three: Double,
      @masked(MaskMode.Regexp("-?\\d*\\.(\\d*)".r)) four: List[Double],
  )

  @derive(loggable)
  final case class Baz(foos: List[Foo] = Nil, ys: Vector[Int] = Vector(), zs: Option[List[List[String]]] = None)

  @derive(loggable)
  final case class MaskedBaz(@masked kek: Option[String], @ignoreOpt a: Option[String] = None)

  @derive(loggable)
  final case class MaskedOptBaz(
      @masked maybeStr: Option[String],
      @masked maybeInt: Option[Int],
      @masked maybeBool: Option[Boolean],
      @masked maybeDouble: Option[Double],
      @masked maybeStr2: Option[String]
  )

  @derive(loggable)
  final case class MaskedCustom(
      @masked(Custom(_ => "*")) sensitiveField: String,
      @masked(Custom(name => name.take(1) + "***")) firstName: Option[String],
      @masked(Custom(i => "*" * i.length())) age: Int
  )

  @derive(loggable)
  final case class MaskedContra(
      @masked(MaskMode.Erase) id: UUID,
      @masked(MaskMode.ForLength(4)) date: LocalDate,
  )
}
