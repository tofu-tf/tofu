package tofu.higherKind.derived

import tofu.higherKind.RepresentableK

import scala.reflect.macros.blackbox

class HigherKindedMacros(override val c: blackbox.Context) extends cats.tagless.DeriveMacros(c) {
  import c.universe._

  implicit final class MethodOps(private val m: Method) {
    def occursInParams(symbol: Symbol): Boolean =
      m.paramLists.exists(_.exists {
        case ValDef(_, _, tpe, _) =>
          tpe.exists(_.tpe.typeSymbol == symbol)
      })
  }

  def tabulate(algebra: Type): (String, Type => Tree) =
    "tabulate" -> {
      case PolyType(List(f), MethodType(List(hom), _)) =>
        val members = overridableMembersOf(algebra)
        val types   = delegateAbstractTypes(algebra, members, algebra)
        val repk    = reify(tofu.higherKind.RepK).tree
        val alg     = TermName(c.freshName("alg"))
        val et      = tq""
        val algv    = q"val $alg: $et"
        val ff = algebra match {
          case PolyType(List(ff1), _) => ff1
        }
        val methods = delegateMethods(algebra, members, NoSymbol) {
          case method if method.occursInParams(ff) =>
            abort(s"Type parameter $ff appears in contravariant position in method ${method.name}")

          case method if method.occursInReturn(ff) =>
            val params = method.paramLists.map(_.map(_.name))
            val body = q"$hom($repk[$algebra](($algv => $alg.${method.name}(...$params))))"
            val tpe = appliedType(f, method.returnType.typeArgs)
            method.copy(body = body, returnType = tpe)

          case method =>
            abort(s"Type parameter $ff does not appear in return in method ${method.name}")
        }

        val res = implement(algebra)(f)(types ++ methods)
        res
    }

  def representableK[Alg[_[_]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[RepresentableK[Alg]](tag)(tabulate, productK, mapK)
}
