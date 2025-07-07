package tofu.logging

import scala.deriving.Mirror

object SingletonEnumLoggable:
  inline def derived[T: Mirror.SumOf]: Loggable[T] =
    SingletonEnumLoggableMacro.ensureSingletonEnum[T]
    Loggable[String].contramap(_.toString)
