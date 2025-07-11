package tofu.logging

import scala.deriving.Mirror

trait SingletonEnumLoggable[A] extends SingleValueLoggable[A]

object SingletonEnumLoggable:
  def makeInstance[T]: SingletonEnumLoggable[T] =
    new SingletonEnumLoggable[T]:
      def logValue(a: T): LogParamValue = StrValue(a.toString)

      override def putField[I, V, R, M](a: T, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
        receiver.addString(name, a.toString, input)

  inline def derived[T: Mirror.SumOf]: SingletonEnumLoggable[T] =
    SingletonEnumLoggableMacro.ensureSingletonEnum[T]
    makeInstance[T]
