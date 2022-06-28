package tofu.optics.macros

import scala.quoted.*

import tofu.optics.Subset

object GenSubset:
  inline def apply[S, A <: S]: Subset[S, A] = ${ mkSubset[S, A] }

def mkSubset[S: Type, A <: S: Type](using qctx: Quotes) =
  import qctx.reflect.*

  (Type.of[S], Type.of[A]) match {
    case ('[s], '[a]) =>
      '{
        Subset[s]((from: s) => if (from.isInstanceOf[a]) Option(from.asInstanceOf[a]) else None)((to: a) =>
          to.asInstanceOf[s]
        )
      }.asExprOf[Subset[S, A]]
  }
