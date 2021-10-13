package tofu.optics.macros

import tofu.optics.Equivalent

import scala.reflect.internal.SymbolTable
import scala.reflect.macros.{blackbox, whitebox}

object GenEquivalent {

  /** Generate an [[Equivalent]] between a case class `S` and its unique field of type `A`. */
  def apply[S, A]: Equivalent[S, A] = macro GenEquivalentImpl.genEquiv_impl[S, A]

  /** Generate an [[Equivalent]] between an object `S` and `Unit`. */
  def unit[S]: Equivalent[S, Unit] = macro GenEquivalentImpl.genEquiv_unit_impl[S]

  /** Generate an [[Equivalent]] between a case class `S` and its fields.
    *
    * Case classes with 0 fields will correspond with `Unit`, 1 with the field type, 2 or more with a tuple of all field
    * types in the same order as the fields themselves.
    *
    * Case classes with multiple parameter-lists (example: `case class X(…)(…)`) are rejected.
    */
  def fields[S]: Equivalent[S, _] = macro GenEquivalentImplW.genEquiv_fields_impl[S]
}

sealed abstract class GenEquivalentImplBase {
  val c: blackbox.Context
  import c.universe._

  protected final def fail(msg: String): Nothing =
    c.abort(c.enclosingPosition, msg)

  protected final def caseAccessorsOf[S: c.WeakTypeTag]: List[MethodSymbol] =
    weakTypeOf[S].decls.collect { case m: MethodSymbol if m.isCaseAccessor => m }.toList

  protected final def genEquiv_unit_tree[S: c.WeakTypeTag]: c.Tree = {
    val sTpe = weakTypeOf[S]

    if (sTpe.typeSymbol.isModuleClass) {
      val table = c.universe.asInstanceOf[SymbolTable]
      val tree  = table.gen
      val obj   = tree.mkAttributedQualifier(sTpe.asInstanceOf[tree.global.Type]).asInstanceOf[Tree]

      val name = s"${sTpe.typeSymbol.name.decodedName}.type<->"
      q"""
        _root_.tofu.optics.Equivalent[${sTpe}]($name)(Function.const(()))(Function.const(${obj}))
      """
    } else {
      val name = s"${sTpe.typeSymbol.name.decodedName}<->"
      caseAccessorsOf[S] match {
        case Nil =>
          val sTpeSym = sTpe.typeSymbol.companion
          q"""
            _root_.tofu.optics.Equivalent[${sTpe}]($name)(Function.const(()))(Function.const(${sTpeSym}()))
          """
        case _   => fail(s"$sTpe needs to be a case class with no accessor or an object.")
      }
    }
  }
}

class GenEquivalentImpl(override val c: blackbox.Context) extends GenEquivalentImplBase {
  import c.universe._

  def genEquiv_impl[S: c.WeakTypeTag, A: c.WeakTypeTag]: c.Expr[Equivalent[S, A]] = {
    val (sTpe, aTpe) = (weakTypeOf[S], weakTypeOf[A])

    val fieldMethod = caseAccessorsOf[S] match {
      case m :: Nil => m
      case Nil      =>
        fail(s"Cannot find a case class accessor for $sTpe, $sTpe needs to be a case class with a single accessor.")
      case _        =>
        fail(s"Found several case class accessor for $sTpe, $sTpe needs to be a case class with a single accessor.")
    }

    val sTpeSym = sTpe.typeSymbol.companion
    val name    = s"${sTpeSym.name.decodedName}<->"

    c.Expr[Equivalent[S, A]](q"""
      import _root_.tofu.optics.Equivalent
      Equivalent[$sTpe]($name)((s: $sTpe) => s.$fieldMethod)((a: $aTpe) => $sTpeSym(a))
    """)
  }

  def genEquiv_unit_impl[S: c.WeakTypeTag]: c.Expr[Equivalent[S, Unit]] =
    c.Expr[Equivalent[S, Unit]](genEquiv_unit_tree[S])
}

class GenEquivalentImplW(override val c: whitebox.Context) extends GenEquivalentImplBase {
  import c.universe._

  protected final def nameAndType(T: Type, s: Symbol): (TermName, Type) = {
    def paramType(name: TermName): Type =
      T.decl(name).typeSignatureIn(T) match {
        case NullaryMethodType(t) => t
        case t                    => t
      }

    val a = s.asTerm.name match {
      case n: TermName => n
      case n           => fail("Expected a TermName, got " + n)
    }
    val A = paramType(a)
    (a, A)
  }

  def genEquiv_fields_impl[S: c.WeakTypeTag]: Tree = {
    val sTpe = weakTypeOf[S]

    val sTpeSym = sTpe.typeSymbol.asClass
    if (!sTpeSym.isCaseClass)
      fail(s"$sTpe is not a case class.")

    val paramLists = sTpe.decls.collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
      .getOrElse(fail(s"Unable to discern primary constructor for $sTpe."))
      .paramLists

    val name = s"${sTpeSym.name.decodedName}<->"

    paramLists match {
      case Nil | Nil :: Nil =>
        genEquiv_unit_tree[S]

      case (param :: Nil) :: Nil =>
        val (pName, pType) = nameAndType(sTpe, param)

        q"""
          import _root_.tofu.optics.Equivalent
          Equivalent[$sTpe]($name)((s: $sTpe) => s.$pName)((a: $pType) => ${sTpeSym.companion}(a))
        """

      case params :: Nil =>
        var readField = List.empty[Tree]
        var readTuple = List.empty[Tree]
        var types     = List.empty[Type]
        for ((param, i) <- params.zipWithIndex.reverse) {
          val (pName, pType) = nameAndType(sTpe, param)
          readField ::= q"s.$pName"
          readTuple ::= q"a.${TermName("_" + (i + 1))}"
          types ::= pType
        }

        q"""
          import _root_.tofu.optics.Equivalent
          Equivalent[$sTpe]($name)(
            (s: $sTpe) => (..$readField)
            )(
            (a: (..$types)) => ${sTpeSym.companion}(..$readTuple)
          )          
        """

      case _ :: _ :: _ =>
        fail(s"Found several parameter-lists for $sTpe, $sTpe needs to be a case class with a single parameter-list.")
    }
  }
}
