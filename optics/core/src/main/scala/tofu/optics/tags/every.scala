package tofu.optics
package tags

import tofu.optics.functions

object every extends TaggerObj[PItems] {
  implicit def everyMap[K, V1, V2]: PTagApply[PItems, Map[K, V1], Map[K, V2], V1, V2, this.type, Unit] =
    _ => functions.mapValuesP

  implicit def everyList[A, B]: PTagApply[PItems, List[A], List[B], A, B, this.type, Unit] =
    _ => functions.listElemsP

  implicit def everyVector[A, B]: PTagApply[PItems, Vector[A], Vector[B], A, B, this.type, Unit] =
    _ => functions.vecElemsP

  implicit def everyTuple2[A, B]: PTagApply[PItems, (A, A), (B, B), A, B, this.type, Unit] = _ => functions.everyTuple2

  implicit def everyTuple3[A, B]: PTagApply[PItems, (A, A, A), (B, B, B), A, B, this.type, Unit] = _ =>
    functions.everyTuple3

  implicit def everyTuple4[A, B]: PTagApply[PItems, (A, A, A, A), (B, B, B, B), A, B, this.type, Unit] = _ =>
    functions.everyTuple4
}
