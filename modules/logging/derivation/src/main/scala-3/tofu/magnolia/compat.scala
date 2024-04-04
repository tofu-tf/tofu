package tofu.magnolia

object compat {
  type Param[Typeclass[_], Type] = magnolia1.CaseClass.Param[Typeclass, Type]

  def deref[Typeclass[_], Type](p: Param[Typeclass, Type]): Type => p.PType = tpe =>
    p.deref(tpe)

}
