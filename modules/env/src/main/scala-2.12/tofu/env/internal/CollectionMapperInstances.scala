package tofu.env.internal

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

class CollectionMapperInstances {
  implicit def iterableOpsMapper[A, B, C[x] <: IterableLike[x, C[x]]](implicit
      cbf: CanBuildFrom[C[A], B, C[B]]
  ): CollectionMapper[A, B, C] =
    new CollectionMapper[A, B, C] {
      def map(ca: C[A], f: A => B): C[B] = ca.map[B, C[B]](f)
    }
}
