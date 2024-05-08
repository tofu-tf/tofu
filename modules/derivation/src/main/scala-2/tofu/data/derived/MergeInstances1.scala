package tofu.data
package derived

import derevo.Derivation
import magnolia1.{CaseClass, Magnolia, SealedTrait}
import tofu.magnolia.compat

trait MergeInstances1 extends Derivation[Merge] {
  type Typeclass[A] = Merge[A]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    (a, b) =>
      caseClass.construct(p => p.typeclass.merge(compat.deref[Typeclass, T](p)(a), compat.deref[Typeclass, T](p)(b)))

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    (a, b) => sealedTrait.split(a) { h => if (h.cast.isDefinedAt(b)) h.typeclass.merge(h.cast(a), h.cast(b)) else a }

  implicit def instance[A]: Merge[A] = macro Magnolia.gen[A]
}
