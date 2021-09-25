package tofu.env.internal
import scala.collection.IterableOps

private[tofu] class CollectionMapperInstances {
  private[this] val mapperAny: CollectionMapper[Any, Any, Iterable]                                =
    new CollectionMapper[Any, Any, Iterable] {
      def map(ca: Iterable[Any], f: Any => Any): Iterable[Any] = ca.map(f)
    }

  implicit def iterableOpsMapper[A, B, C[x] <: IterableOps[x, C, C[x]]]: CollectionMapper[A, B, C] =
    mapperAny.asInstanceOf[CollectionMapper[A, B, C]]
}
