package tofu.logging

import scala.quoted.*

object SingletonEnumLoggableMacro:

  inline def getSingletonEnumCases[T]: List[String] =
    ${ getSingletonEnumCases[T] }

  def getSingletonEnumCases[T: Type](using quotes: Quotes): Expr[List[String]] =
    import quotes.reflect.*

    val enumSymbol = TypeRepr.of[T].typeSymbol

    val enumCaseNames = enumSymbol.children.map: caseSymbol =>
      if (caseSymbol.caseFields.nonEmpty) then
        report.errorAndAbort(
          s"${enumSymbol.fullName} has non-singleton case ${caseSymbol.name}, therefore SingletonEnumLoggable instance can not be derived"
        )
      caseSymbol.name

    Expr(enumCaseNames)
