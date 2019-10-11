package tofu.higherKind.derived

import cats.tagless.InvariantK
import org.manatki.derevo.{Derevo, DerivationK2, delegating}
import tofu.higherKind.RepresentableK

import scala.reflect.macros.blackbox

class HigherKindedMacros(override val c: blackbox.Context) extends cats.tagless.DeriveMacros(c) {
  import c.universe._

  def tabulate(algebra: Type): (String, Type => Tree) =
    "tabulate" -> {
      case PolyType(List(f), MethodType(List(hom), _)) =>
        val members = overridableMembersOf(algebra)
        val types   = delegateAbstractTypes(algebra, members, algebra)
        val repk    = reify(tofu.higherKind.RepK).tree
        val alg     = TermName(c.freshName("alg"))
        val et      = tq""
        val algv    = q"val $alg: $et"
        val methods = delegateMethods(algebra, members, algebra.typeSymbol) {
          case method if method.occursInSignature(f) =>
            abort(s"Type parameter $f appears in contravariant position in method ${method.name}")
          case method =>
            val params = method.paramLists.map(_.map(_.name))
            val body   = q"$hom($repk[$algebra](($algv => $alg.${method.name}(...$params))))"
            method.copy(body = body, returnType = NoType)

        }

        val res = implement(algebra)(f)(types ++ methods)
        res
    }

  def representableK[Alg[_[_]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[RepresentableK[Alg]](tag)(tabulate)
}


@delegating("tofu.higherKind.derived.genRepresentableK")
object representableK extends DerivationK2[RepresentableK]{
  def instance[T[_[_]]]: RepresentableK[T] = macro Derevo.delegateK2[RepresentableK, T]
}
