package tofu.optics.macros

import scala.quoted.*

import tofu.optics.Contains
import tofu.optics.macros.internal.*
import scala.annotation.tailrec

object GenContains:
  def apply[A]: MkContains[A] = new MkContains[A]

class MkContains[A]:
  inline def apply[B](inline expr: A => B): Contains[A, B] = ${ mkContains('expr) }

def mkContains[A: Type, B: Type](expr: Expr[A => B])(using qctx: Quotes) =
  import qctx.reflect.*

  val (_, term) = unwrapLambda(expr.asTerm)

  term match {
    case Select(CaseClass(cc), name) =>
      val ccType    = cc.tpe.widen.dealias
      val fieldType =
        ccType.classSymbol
          .map(_.fieldMembers.find(_.name.trim == name).getOrElse(Symbol.noSymbol))
          .flatMap {
            case FieldType(ta) => Some(ta)
            case _             => None
          }
          .getOrElse(quotes.reflect.report.errorAndAbort("Can't get field type"))

      (ccType.asType, fieldType.asType) match {
        case ('[a], '[b]) =>
          '{
            Contains[a]((from: a) => ${ Select.unique('{ from }.asTerm, name).asExprOf[b] })((from: a, to: b) =>
              ${ Select.overloaded('{ from }.asTerm, "copy", Nil, NamedArg(name, '{ to }.asTerm) :: Nil).asExprOf[a] }
            )
          }.asExprOf[Contains[A, B]]
      }
  }
