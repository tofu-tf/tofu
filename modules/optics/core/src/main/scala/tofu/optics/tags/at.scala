package tofu.optics
package tags

import tofu.optics.functions

object at extends TaggerObj[PContains] {
  implicit def atMap[K, V]: TagApply[PContains, Map[K, V], Option[V], this.type, K] =
    k => functions.mapAt[K, V](k)

  implicit def atSet[A]: TagApply[PContains, Set[A], Boolean, this.type, A] =
    i => functions.setAt[A](i)
}
