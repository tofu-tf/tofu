package tofu.logging

object SingletonEnumLoggable:
  inline def derived[T <: scala.reflect.Enum]: Loggable[T] =
    SingletonEnumLoggableMacro.ensureSingletonEnum[T]
    Loggable[String].contramap(_.toString)
