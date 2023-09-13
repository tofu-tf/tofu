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
    mkDelegateTyped[X](path, TypeTree.of[F])

  def mkDelegate1_0[F[_]: Type, E: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegateTyped[X](path, TypeTree.of[F], TypeTree.of[E])

  def mkDelegate0_1_p[T: Type, F[_]: Type, P, X: Type](path: Expr[String], p: Expr[P])(using Quotes): Expr[X] =
    import quotes.reflect.*
    val typed = mkDelegateTyped[X](path, TypeTree.of[T], TypeTree.of[F])
    Apply(typed.asTerm, p.asTerm :: Nil).asExprOf[X]

  def mkDelegate2[I[_]: Type, F[_]: Type, X: Type](path: Expr[String])(using Quotes): Expr[X] =
    import quotes.reflect.*
    mkDelegateTyped[X](path, TypeTree.of[I], TypeTree.of[F])

  private def mkDelegateTyped[X: Type](using
      Quotes
  )(path: Expr[String], tps: quotes.reflect.TypeTree*): Expr[X] =
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
    expr

}
