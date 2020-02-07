package tofu.higherKind.derived

import tofu.higherKind.{Embed, RepresentableK}

import scala.reflect.macros.blackbox

class HigherKindedMacros(override val c: blackbox.Context) extends cats.tagless.DeriveMacros(c) {
  import c.internal._
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
            val body   = q"$hom($repk[$algebra](($algv => $alg.${method.name}(...$params))))"
            val tpe    = appliedType(f, method.returnType.typeArgs)
            method.copy(body = body, returnType = tpe)

          case method =>
            abort(s"Type parameter $ff does not appear in return in method ${method.name}")
        }

        val res = implement(algebra)(f)(types ++ methods)
        res
    }

  def embedf(algebra: Type): (String, Type => Tree) =
    "embed" -> {
      case PolyType(List(f), MethodType(List(faf), MethodType(List(fmonad), _))) =>
        def makeMethod(method: Method)(body: List[List[TermName]] => Tree): Method = {
          val params = method.paramLists.map(_.map(_.name))
          method.copy(body = body(params))
        }

        val Af = singleType(NoPrefix, faf).widen match {
          case TypeRef(_, _, List(af1)) => af1
          case other =>
            abort(s"something wrong with $faf parameter, this should not happen, also I hate macros")
        }

        val members = overridableMembersOf(Af)
        val types   = delegateAbstractTypes(Af, members, Af)

        val methods = delegateMethods(Af, members, faf) {
          case method if method.returnType.typeConstructor.typeSymbol == f =>
            makeMethod(method)(params => q"$fmonad.flatMap($faf)(_.${method.name}(...$params))")
          case method if method.occursInReturn(f) =>
            makeMethod(method) { params =>
              val F  = summon[Embed[Any]](polyType(f :: Nil, method.returnType))
              q"$F.embed($fmonad.map($faf)(_.${method.name}(...$params)))"
            }
          case method =>
            abort(s"Type parameter $f does not appear in return in method ${method.name}")
        }

        implement(Af)(f)(types ++ methods)
    }

  def representableK[Alg[_[_]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[RepresentableK[Alg]](tag)(tabulate, productK, mapK)

  def embed[Alg[_[_]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[Embed[Alg]](tag)(embedf)
}
