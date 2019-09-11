package tofu.env.internal

abstract class CollectionMapper[A, B, C[_]]{
    def map(ca: C[A], f: A => B): C[B]
}

object CollectionMapper extends CollectionMapperInstances

