package tofu.logging

import scala.deriving.Mirror

trait SingletonEnumLoggable[A] extends SingleValueLoggable[A]

object SingletonEnumLoggable:
  private def makeInstance[T: Mirror.SumOf](enumCaseNames: List[String]): SingletonEnumLoggable[T] =
    def getCaseName[T](v: T)(using m: Mirror.SumOf[T]): String =
      val ordIndex = m.ordinal(v)
      enumCaseNames(ordIndex)

    new SingletonEnumLoggable[T]:
      def logValue(a: T): LogParamValue = StrValue(getCaseName(a))

      override def putField[I, V, R, M](a: T, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
        receiver.addString(name, getCaseName(a), input)

  inline def derived[T: Mirror.SumOf]: SingletonEnumLoggable[T] =
    val enumCaseNames = SingletonEnumLoggableMacro.getSingletonEnumCases[T]
    makeInstance[T](enumCaseNames)
