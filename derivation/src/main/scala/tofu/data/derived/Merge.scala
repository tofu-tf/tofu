package tofu.data
package derived

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

import cats.kernel.Semigroup
import magnolia.{CaseClass, Magnolia, SealedTrait}
import simulacrum.typeclass
import derevo.Derivation

@typeclass trait Merge[A] {
  def merge(a: A, b: A): A
}

trait MergeInstances1 {
  type Typeclass[A] = Merge[A]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    (a, b) => caseClass.construct(p => p.typeclass.merge(p.dereference(a), p.dereference(b)))

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    (a, b) => sealedTrait.dispatch(a) { h => if (h.cast.isDefinedAt(b)) h.typeclass.merge(h.cast(a), h.cast(b)) else a }

  implicit def instance[A]: Merge[A] = macro Magnolia.gen[A]
}

object Merge extends Derivation[Merge] with MergeInstances1 {
  implicit def optionInstance[A](implicit m: Merge[A]): Merge[Option[A]] =
    (ao, bo) => ao.fold(bo)(a => bo.fold(ao)(b => Some(m.merge(a, b))))

  implicit def primitiveInstance[A: Primitive]: Merge[A] = (a: A, _: A) => a

  sealed class Primitive[A]
  final implicit object primitiveByte          extends Primitive[Byte]
  final implicit object primitiveShort         extends Primitive[Short]
  final implicit object primitiveInt           extends Primitive[Int]
  final implicit object primitiveLong          extends Primitive[Long]
  final implicit object primitiveChar          extends Primitive[Char]
  final implicit object primitiveFloat         extends Primitive[Float]
  final implicit object primitiveDouble        extends Primitive[Double]
  final implicit object primitiveUnit          extends Primitive[Unit]
  final implicit object primitiveBigDecimal    extends Primitive[BigDecimal]
  final implicit object primitiveBigInt        extends Primitive[BigInt]
  final implicit object primitiveLocalDateTime extends Primitive[LocalDateTime]
  final implicit object primitiveZonedDateTime extends Primitive[ZonedDateTime]
  final implicit object primitiveLocalDate     extends Primitive[LocalDate]
  final implicit object primitiveInstant       extends Primitive[Instant]
  final implicit object primitiveString        extends Primitive[String]
}

object Merged {
  trait OpaqueTag extends Any
  type Base = Any { type MergedOpaque }

  type Mer[A] <: Base with OpaqueTag

  def apply[A](value: A): Mer[A] = value.asInstanceOf[Mer[A]]

  implicit final class MergedOps[A](private val mer: Mer[A]) extends AnyVal {
    def value: A = mer.asInstanceOf[A]
  }

  implicit def mergedSemigroup[A: Merge]: Semigroup[Merged[A]] =
    (x, y) => apply(Merge[A].merge(x.value, y.value))
}
