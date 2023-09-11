package tofu.internal

import scala.quoted.*

object Interop {

  inline def delegate1[F[_], X](inline path: String) =
    ${ mkDelegate1[F, X]('path) }

  inline def delegate1_0[F[_], E, X](inline path: String) =
    ${ mkDelegate1_0[F, E, X]('path) }

  inline def delegate2[I[_], F[_], X](inline path: String) =
    ${ mkDelegate2[F, I, X]('path) }

  def mkDelegate1[F[_]: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegate[X](path, TypeTree.of[F])

  def mkDelegate1_0[F[_]: Type, E: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegate[X](path, TypeTree.of[F], TypeTree.of[E])

  def mkDelegate2[I[_]: Type, F[_]: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegate[X](path, TypeTree.of[I], TypeTree.of[F])

  def mkDelegate[X: Type](using Quotes)(path: Expr[String], tps: quotes.reflect.TypeTree*): Expr[X] =
    import quotes.reflect.*
    val sym     = Symbol.requiredMethod(path.valueOrAbort)
    var iterSym = sym
    val acc     = List.newBuilder[Symbol]

    while !iterSym.isNoSymbol do
      val sym = if iterSym.isClassDef then iterSym.companionModule else iterSym
      acc += sym
      iterSym = iterSym.owner

    val root :: tail = acc.result().reverse
    val start: Term  = Ident(root.termRef)
    val wtf          = tail.foldLeft(start) { (acc, s) => acc.select(s) }
    val out          = TypeApply(wtf, tps.toList)
    val expr         = out.asExprOf[X]
    println(expr.show)
    expr

}
