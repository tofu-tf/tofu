package tofu.data.derived

import magnolia1.*
import tofu.magnolia.compat
import scala.deriving.Mirror

trait MergeInstances1 extends AutoDerivation[Merge] {
  type Typeclass[A] = Merge[A]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    (a, b) =>
      caseClass.construct(p => p.typeclass.merge(compat.deref[Typeclass, T](p)(a), compat.deref[Typeclass, T](p)(b)))

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    (a, b) => sealedTrait.choose(a) { h => if (h.cast.isDefinedAt(b)) h.typeclass.merge(h.cast(a), h.cast(b)) else a }

  inline def instance[A](using Mirror.Of[A]): Merge[A] = autoDerived[A]
}
