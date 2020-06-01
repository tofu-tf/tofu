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

    c.Expr[Subset[S, A]](q"""
      import _root_.tofu.optics.Subset

      new Subset[$sTpe, $aTpe]{
        override def narrow(s: $sTpe): $sTpe Either $aTpe =
          if(s.isInstanceOf[$aTpe]) Right(s.asInstanceOf[$aTpe])
          else Left(s)

        override def upcast(a: $aTpe): $sTpe =
          a.asInstanceOf[$sTpe]
      }
    """)
  }
}
