package tofu.optics.macros.internal

import tofu.optics.{Contains, PContains}

import scala.reflect.macros.blackbox

object Macro {
  def mkContains[S, T, A, B](fieldName: String): PContains[S, T, A, B] = macro MacroImpl.mkContains_impl[S, T, A, B]
}

private[macros] class MacroImpl(val c: blackbox.Context) {
  def genContains_impl[S: c.WeakTypeTag, A: c.WeakTypeTag](field: c.Expr[S => A]): c.Expr[Contains[S, A]] = {
    import c.universe._

    /** Extractor for member select chains. e.g.: SelectChain.unapply(a.b.c) == Some("a",Seq(a.type -> "b", a.b.type ->
      * "c"))
      */
    object SelectChain {
      def unapply(tree: Tree): Option[(Name, Seq[(Type, TermName)])] = tree match {
        case Select(tail @ Ident(termUseName), field: TermName) =>
          Some((termUseName, Seq(tail.tpe.widen -> field)))
        case Select(tail, field: TermName)                      =>
          SelectChain
            .unapply(tail)
            .map(t => t.copy(_2 = t._2 :+ (tail.tpe.widen -> field)))
        case _                                                  => None
      }
    }

    field match {
      // _.field
      case Expr(
            Function(
              List(ValDef(_, termDefName, _, EmptyTree)),
              Select(Ident(termUseName), fieldNameName)
            )
          ) if termDefName.decodedName.toString == termUseName.decodedName.toString =>
        val fieldName = fieldNameName.decodedName.toString
        mkContains_impl[S, S, A, A](c.Expr[String](q"$fieldName"))

      // _.field1.field2...
      case Expr(
            Function(
              List(ValDef(_, termDefName, _, EmptyTree)),
              SelectChain(termUseName, typesFields)
            )
          ) if termDefName.decodedName.toString == termUseName.decodedName.toString =>
        c.Expr[Contains[S, A]](
          typesFields.map { case (t, f) => q"_root_.tofu.optics.macros.GenContains[$t](_.$f)" }
            .reduce((a, b) => q"$a andThen $b")
        )

      case _ =>
        c.abort(
          c.enclosingPosition,
          s"Illegal field reference ${show(field.tree)}; please use _.field1.field2... instead"
        )
    }
  }

  def mkContains_impl[S: c.WeakTypeTag, T: c.WeakTypeTag, A: c.WeakTypeTag, B: c.WeakTypeTag](
      fieldName: c.Expr[String]
  ): c.Expr[PContains[S, T, A, B]] = {
    import c.universe._

    val (sTpe, tTpe, aTpe, bTpe) = (weakTypeOf[S], weakTypeOf[T], weakTypeOf[A], weakTypeOf[B])

    val strFieldName = c.eval(c.Expr[String](c.untypecheck(fieldName.tree.duplicate)))

    val fieldMethod = sTpe.decls.collectFirst {
      case m: MethodSymbol if m.isCaseAccessor && m.name.decodedName.toString == strFieldName => m
    }.getOrElse(c.abort(c.enclosingPosition, s"Cannot find method $strFieldName in $sTpe"))

    val constructor = sTpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.getOrElse(c.abort(c.enclosingPosition, s"Cannot find constructor in $sTpe"))

    val field = constructor.paramLists.head
      .find(_.name.decodedName.toString == strFieldName)
      .getOrElse(c.abort(c.enclosingPosition, s"Cannot find constructor field named $fieldName in $sTpe"))

    val name = s"_.$strFieldName"
    c.Expr[PContains[S, T, A, B]](q"""
      import _root_.tofu.optics.PContains
      import _root_.scala.language.higherKinds // prevent warning at call site

      PContains[$sTpe, $bTpe][$aTpe, $tTpe]($name)((s : $sTpe) => s.$fieldMethod)((s: $sTpe, a: $bTpe) => s.copy($field = a))
    """)
  }
}
