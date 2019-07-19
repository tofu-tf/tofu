package tofu.logging

import enumeratum.EnumEntry

trait LoggableEnum[E <: EnumEntry]  {
  final implicit val logging: Loggable[E] = Loggable.stringValue.contramap(_.entryName)
}
