package tofu.higherKind.derived

import tofu.higherKind.RepresentableK
import quoted.*

object representableK:

  inline def derived[U[_[_]]]: RepresentableK[U] = ${ representableKMacro[U] }

  def representableKMacro[U[_[_]]: Type](using Quotes): Expr[RepresentableK[U]] = ???
