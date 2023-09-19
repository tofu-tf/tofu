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
    ${ mkDelegate2[F, I, X]('path) }

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

    val sym = Symbol.requiredMethod(path.valueOrAbort)
    println("|||termRef:" + sym.termRef.show)
    // println("|||tree:" + sym.tree.show)
    // println("|||tree.getClas:" + sym.tree.getClass())

    val symExists =
      try {
        sym.tree
        true
      } catch { case _ => false }

    if !symExists then
      println("!sym.exists")
      report.errorAndAbort("Boom")
    else
      val wtf              = Ident(sym.termRef)
      val withTypes        = wtf.appliedToTypeTrees(tps.toList)
      val withExplicitArgs =
        if args.nonEmpty then withTypes.appliedToArgs(args)
        else withTypes

      // println("AAA1" + withTypes.tpe.show)
      // println("AAA2" + withTypes.tpe)
      // println("BBB1" + withTypes.etaExpand(sym).show)
      // println("BBB1" + withTypes.etaExpand(sym))

      val withImplicitArgs = withExplicitArgs.tpe match
        case t: LambdaType =>
          println("YY" + t.paramNames + " | " + t.paramTypes + " | " + t.resType.show)
          val summonedInst = t.paramTypes.map(t =>
            Implicits.search(t) match
              case result: ImplicitSearchSuccess => result.tree
              case _                             => report.errorAndAbort(s"Cannot find an implicit instance for type ${t.asType}! Please help!")
          )
          withExplicitArgs.appliedToArgs(summonedInst)
        case _             =>
          println("NO")
          withExplicitArgs

      val expr = withImplicitArgs.asExprOf[X]
      expr
}
