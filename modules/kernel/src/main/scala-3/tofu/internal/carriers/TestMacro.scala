package tofu.internal.carriers

import scala.quoted.*

object TestMacro {

  inline def delegate0[F[_], X[_[_]]](inline path: String) = 
    ${ mkDelegate[F, X]('path)}

  def mkDelegate[F[_]: Type, X[_[_]]: Type](path: Expr[String])(using Quotes): Expr[X[F]] = 
    import quotes.reflect._
    val sym = Symbol.requiredMethod(path.valueOrAbort)
    var iterSym = sym
    val acc = List.newBuilder[Symbol]

    while !iterSym.isNoSymbol do
      val sym = if iterSym.isClassDef then iterSym.companionModule else iterSym
      acc += sym
      iterSym = iterSym.owner

    val root :: tail = acc.result().reverse
    val start: Term = Ident(root.termRef)
    val wtf = tail.foldLeft(start){(acc, s) => acc.select(s)}
    val out = TypeApply(wtf, List(TypeTree.of[F]))
    val expr = out.asExprOf[X[F]]
    expr



}
