package tofu.optics
package tags

import tofu.optics.functions

object index extends TaggerObj[PProperty] {
  implicit def indexMap[K, V]: TagApply[PProperty, Map[K, V], V, this.type, K] =
    k => functions.mapItem[K, V](k)

  implicit def indexList[A]: TagApply[PProperty, List[A], A, this.type, Int] =
    i => functions.listItem[A](i)

  implicit def indexVector[A]: TagApply[PProperty, Vector[A], A, this.type, Int] =
    i => functions.vecItem[A](i)
}
