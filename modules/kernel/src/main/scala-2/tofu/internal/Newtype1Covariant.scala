package tofu.internal

trait Newtype1Covariant {
  type Base = Any { type newTypeCovariant }
  trait Tag extends Any
  type Type[+A] <: Base with Tag
}
