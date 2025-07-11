package tofu.logging

import scala.quoted.*

object SingletonEnumLoggableMacro:

  inline def ensureSingletonEnum[T]: Unit =
    ${ ensureSingletonEnumMacro[T] }

  def ensureSingletonEnumMacro[T: Type](using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*

    val enumSymbol = TypeRepr.of[T].typeSymbol

    enumSymbol.children.foreach: caseSymbol =>
      if (caseSymbol.caseFields.nonEmpty) then
        report.errorAndAbort(
          s"${enumSymbol.fullName} has non-singleton case ${caseSymbol.name}, therefore SingletonEnumLoggable instance can not be derived"
        )

    '{ () }
