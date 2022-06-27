package tofu.optics.macros.internal

import scala.quoted.*

object AsTerm:
  def unapply(using quotes: Quotes)(expr: Expr[Any]): Option[quotes.reflect.Term] = {
    import quotes.reflect.*
    Some(expr.asTerm)
  }

object AnonfunBlock:
  def unapply(using quotes: Quotes)(term: quotes.reflect.Term): Option[(String, quotes.reflect.Term)] = {
    import quotes.reflect.*
    term match {
      case Lambda(ValDef(paramName, _, _) :: Nil, rhs) => Some((paramName, rhs))
      case _                                           => None
    }
  }

object CaseClass:
  def unapply(using quotes: Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
    term.tpe.classSymbol.flatMap { sym =>
      Option.when(sym.flags.is(quotes.reflect.Flags.Case))(term)
    }

object FieldType:
  def unapply(using quotes: Quotes)(fieldSymbol: quotes.reflect.Symbol): Option[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    fieldSymbol match {
      case sym if sym.isNoSymbol => None
      case sym                   =>
        sym.tree match {
          case ValDef(_, typeTree, _) => Some(typeTree.tpe)
          case _                      => None
        }
    }

def showTerm(using quotes: Quotes)(term: quotes.reflect.Term) =
  term.show(using quotes.reflect.Printer.TreeStructure)

def unwrapLambda(using quotes: Quotes)(input: quotes.reflect.Term): (String, quotes.reflect.Term) =
  input match {
    case quotes.reflect.Inlined(_, _, expansion) => unwrapLambda(expansion)
    case AnonfunBlock(paramName, body)           => (paramName, body)
    case _                                       => quotes.reflect.report.errorAndAbort(s"Expected a lambda, got ${showTerm(input)}")
  }

def getSuppliedTypeArgs(using quotes: Quotes)(fromType: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
  fromType match {
    case quotes.reflect.AppliedType(_, argTypeReprs) => argTypeReprs
    case _                                           => Nil
  }
