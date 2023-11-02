package tofu.internal

import scala.quoted.*

object Interop {

  inline def delegate1[F[_], X](inline path: String) =
    ${ mkDelegate1[F, X]('path) }

  inline def delegate1_0[F[_], E, X](inline path: String) =
    ${ mkDelegate1_0[F, E, X]('path) }

  inline def delegate0_1_p[T, F[_], P, X](inline path: String, inline p: P) =
    ${ mkDelegate0_1_p[T, F, P, X]('path, 'p) }

  inline def delegate2[I[_], F[_], X](inline path: String) =
    ${ mkDelegate2[I, F, X]('path) }

  def mkDelegate1[F[_]: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegateImpl[X](path, Nil, TypeTree.of[F])

  def mkDelegate1_0[F[_]: Type, E: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegateImpl[X](path, Nil, TypeTree.of[F], TypeTree.of[E])

  def mkDelegate0_1_p[T: Type, F[_]: Type, P, X: Type](path: Expr[String], p: Expr[P])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegateImpl[X](path, p.asTerm :: Nil, TypeTree.of[T], TypeTree.of[F])

  def mkDelegate2[I[_]: Type, F[_]: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegateImpl[X](path, Nil, TypeTree.of[I], TypeTree.of[F])

  private def mkDelegateImpl[X: Type](using
      Quotes
  )(path: Expr[String], args: List[quotes.reflect.Term], tps: quotes.reflect.TypeTree*): Expr[X] =
    import quotes.reflect.*
    try {
      val sym       = Symbol.requiredMethod(path.valueOrAbort)
      val symExists =
        try {
          sym.tree
          true
        } catch { case _ => false }

      if !symExists then
        report.errorAndAbort("Symbol does not exists")
      else
        val wtf              = Ident(sym.termRef)
        val withTypes        = wtf.appliedToTypeTrees(tps.toList)
        val withExplicitArgs =
          if args.nonEmpty then withTypes.appliedToArgs(args)
          else withTypes

        val withImplicitArgs = withExplicitArgs.tpe match
          case t: LambdaType =>
            val summonedInst = t.paramTypes.map(t =>
              Implicits.search(t) match
                case result: ImplicitSearchSuccess =>
                  result.tree
                case _                             =>
                  report.errorAndAbort(
                    s"Applying definition need extra implicit arguments. Cannot find an implicit instance for type ${t.show}."
                  )
            )
            withExplicitArgs.appliedToArgs(summonedInst)
          case _             =>
            withExplicitArgs

        // for some reason lambda type like this `FinallyCarrier2.Aux[F, Throwable, [x] =>> CE2Kernel.CEExit[java.lang.Throwable, x]]`
        // has no subtyping relations with type `FinallyCarrier2.Aux[F, Throwable, [x] =>> Any]` and `.asExprOf[X]` throws java.lang.Exception: Expr cast exception

        // For such cases old and classic `asInstanceOf` helps with extra casting.

        val asInstanceOf = TypeRepr.of[AnyRef].typeSymbol.methodMember("asInstanceOf").head
        val casted       =
          withImplicitArgs
            .select(asInstanceOf)
            .appliedToType(TypeRepr.of[X])

        casted.asExprOf[X]
    } catch {
      case e: scala.quoted.runtime.StopMacroExpansion =>
        throw e
      case err                                        =>
        println("ERROR: " + err)
        report.errorAndAbort("Error in macros: " + err)
    }
}
