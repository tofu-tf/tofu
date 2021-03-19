package tofu.optics.macros

import tofu.optics.Subset

import scala.reflect.macros.blackbox

object GenSubset {

  /** generate a [[Subset]] between `S` and a subtype `A` of `S` */
  def apply[S, A <: S]: Subset[S, A] = macro GenSubsetImpl.genSubset_impl[S, A]
}

private class GenSubsetImpl(val c: blackbox.Context) {
  def genSubset_impl[S: c.WeakTypeTag, A: c.WeakTypeTag]: c.Expr[Subset[S, A]] = {
    import c.universe._

    val (sTpe, aTpe) = (weakTypeOf[S], weakTypeOf[A])
    val aName        = ":" + (aTpe: Type).typeSymbol.name.decodedName.toString()

    c.Expr[Subset[S, A]](q"""
      _root_.tofu.optics.PSubset[$sTpe, $aTpe]($aName)(
        (s: $sTpe) => 
          if(s.isInstanceOf[$aTpe]) Right(s.asInstanceOf[$aTpe])
          else Left(s)
        )((a: $aTpe) => a.asInstanceOf[$sTpe])      
    """)
  }
}
