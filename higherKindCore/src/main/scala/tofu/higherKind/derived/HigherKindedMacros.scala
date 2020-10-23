package tofu.higherKind.derived

import tofu.higherKind.{Embed, RepresentableK}
import tofu.higherKind.bi.{EmbedBK, RepresentableB}

import scala.reflect.macros.blackbox

class HigherKindedMacros(override val c: blackbox.Context) extends cats.tagless.DeriveMacros(c) {
  import c.internal._
  import c.universe._

  trait TabulateParams {
    def repk: Tree
    def tabMethod(retConst: Type): Tree
  }

  trait EmbedParams {
    def embMethod(retConst: Type): Tree
    def join(instance: Symbol, arg: Symbol, lam: Tree): Tree
    def joinEmb(instance: Symbol, arg: Symbol, lam: Tree): Tree
  }

  implicit final class MethodOps(private val m: Method) {
    def occursInParams(symbol: Symbol): Boolean =
      m.paramLists.exists(_.exists { case ValDef(_, _, tpe, _) =>
        tpe.exists(_.tpe.typeSymbol == symbol)
      })
  }

  // copied from cats.tagless.DeriveMacros.delegateMethods
  // for correct handling of vararg parameters
  private def methodArgs(method: Method, algebra: Type) =
    for (ps <- method.signature.paramLists) yield for (p <- ps) yield p.typeSignatureIn(algebra) match {
      case RepeatedParam(_) => q"${p.name.toTermName}: _*"
      case _                => Ident(p.name)
    }

  def unimplementedMembersOf(tpe: Type): Iterable[Symbol] = overridableMembersOf(tpe).filter(_.isAbstract)

  // copied from the old version of cats.tagless.DeriveMacros
  private def summon[A: TypeTag](typeArgs: Type*): Tree = {
    val tpe = appliedType(typeOf[A].typeConstructor, typeArgs: _*)
    c.inferImplicitValue(tpe).orElse(abort(s"could not find implicit value of type $tpe"))
  }

  private def tabulateTemplate(algebra: Type)(impl: TabulateParams): Type => Tree = {
    case PolyType(List(f), MethodType(List(hom), af)) =>
      val members = unimplementedMembersOf(af)
      val types   = delegateAbstractTypes(af, members, af)
      val repk    = impl.repk
      val alg     = TermName(c.freshName("alg"))
      val rep     = TermName(c.freshName("rep"))
      val et      = tq""
      val algv    = q"val $alg: $et"
      val repv    = q"val $rep: $et"

      val methods = delegateMethods(af, members, NoSymbol) {
        case method if method.occursInParams(f) =>
          abort(s"Type parameter $f appears in contravariant position in method ${method.name}")

        case method if method.returnType.typeConstructor.typeSymbol == f =>
          val params = methodArgs(method, algebra)
          val body   = q"$hom($repk[$algebra](($algv => $alg.${method.name}(...$params))))"
          method.copy(body = body)

        case method if method.occursInReturn(f) =>
          val params = methodArgs(method, algebra)
          val tt     = polyType(f :: Nil, method.returnType)
          val tab    = impl.tabMethod(tt)
          val body   =
            q"$tab($repv => $hom($repk[$algebra]($algv => $rep($alg.${method.name}(...$params)))))"
          method.copy(body = body)

        case method =>
          abort(s"Type parameter $f does not appear in return in method ${method.name}")
      }

      val res = implement(algebra)(f)(types ++ methods)
      res
  }

  private def embedTemplate(algebra: Type)(impl: EmbedParams): Type => Tree = {
    case PolyType(List(f), MethodType(List(faf), MethodType(List(fmonad), _))) =>
      def makeMethod(method: Method)(body: List[List[Tree]] => Tree): Method = {
        val params = methodArgs(method, algebra)
        method.copy(body = body(params))
      }

      val Af = singleType(NoPrefix, faf).widen match {
        case TypeRef(_, _, List(af1)) => af1
        case other @ _                =>
          abort(s"something wrong with $faf parameter, this should not happen, also I hate macros")
      }

      val members = unimplementedMembersOf(Af)
      val types   = delegateAbstractTypes(Af, members, Af)

      val methods = delegateMethods(Af, members, faf) {
        case method if method.returnType.typeConstructor.typeSymbol == f =>
          makeMethod(method) { params =>
            val fm = q"_.${method.name}(...$params)"
            q"${impl.join(fmonad, faf, fm)}"
          }
        case method if method.occursInReturn(f)                          =>
          makeMethod(method) { params =>
            val embedMeth = impl.embMethod(polyType(f :: Nil, method.returnType))
            val fm        = q"_.${method.name}(...$params)"
            val sub       = impl.joinEmb(fmonad, faf, fm)
            q"$embedMeth($sub)"
          }
        case method                                                      =>
          abort(s"Type parameter $f does not appear in return in method ${method.name}")
      }

      implement(Af)(f)(types ++ methods)
  }

  def tabulate(algebra: Type): (String, Type => Tree) =
    "tabulate" -> tabulateTemplate(algebra)(new TabulateParams {
      def repk                            = reify(tofu.higherKind.RepK).tree
      def tabMethod(retConst: Type): Tree = q"${summon[RepresentableK[Any]](retConst)}.tab"
    })

  def bitabulate(algebra: Type): (String, Type => Tree) =
    "bitabulate" -> tabulateTemplate(algebra)(new TabulateParams {
      def repk                            = reify(tofu.higherKind.bi.RepBK).tree
      def tabMethod(retConst: Type): Tree = q"${summon[RepresentableB[Any]](retConst)}.tab"
    })

  def embedf(algebra: Type): (String, Type => Tree) =
    "embed" -> embedTemplate(algebra)(new EmbedParams {
      def join(instance: Symbol, arg: Symbol, lam: Tree): Tree    = q"$instance.flatMap($arg)($lam)"
      def joinEmb(instance: Symbol, arg: Symbol, lam: Tree): Tree = q"$instance.map($arg)($lam)"
      def embMethod(retConst: Type): Tree                         = q"${summon[Embed[Any]](retConst)}.embed"
    })

  def biembed(algebra: Type): (String, Type => Tree) =
    "biembed" -> embedTemplate(algebra)(new EmbedParams {
      override def join(instance: Symbol, arg: Symbol, lam: Tree): Tree    = q"$instance.foldWith($arg)($lam)($lam)"
      override def joinEmb(instance: Symbol, arg: Symbol, lam: Tree): Tree = q"$instance.bimap($lam, $lam)"
      override def embMethod(retConst: Type): Tree                         = q"${summon[EmbedBK[Any]](retConst)}.biembed"
    })

  def representableK[Alg[_[_]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[RepresentableK[Alg]](tag)(tabulate, productK, mapK, embedf)

  def embed[Alg[_[_]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[Embed[Alg]](tag)(embedf)

  def representableB[Alg[_[_, _]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[RepresentableB[Alg]](tag)(bitabulate, biembed)

  def embedB[Alg[_[_, _]]](implicit tag: WeakTypeTag[Alg[Any]]): Tree =
    instantiate[EmbedBK[Alg]](tag)(biembed)
}
