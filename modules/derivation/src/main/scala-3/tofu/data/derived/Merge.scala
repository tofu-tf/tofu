package tofu.data.derived

import cats.kernel.Semigroup
import magnolia1.*
import tofu.compat.unused
import tofu.internal.DataComp
import tofu.magnolia.compat
import scala.deriving.Mirror

import java.time.{Instant, LocalDate, LocalDateTime, ZonedDateTime}

trait Merge[A] {
  def merge(a: A, b: A): A
}

trait MergeInstances1 {
  type Typeclass[A] = Merge[A]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    (a, b) =>
      caseClass.construct(p => p.typeclass.merge(compat.deref[Typeclass, T](p)(a), compat.deref[Typeclass, T](p)(b)))

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    (a, b) => sealedTrait.choose(a) { h => if (h.cast.isDefinedAt(b)) h.typeclass.merge(h.cast(a), h.cast(b)) else a }

}

object Merge extends AutoDerivation[Merge] with MergeInstances1 with DataComp[Merge] {
  implicit def optionInstance[A](implicit m: Merge[A]): Merge[Option[A]] =
    (ao, bo) => ao.fold(bo)(a => bo.fold(ao)(b => Some(m.merge(a, b))))

  implicit def primitiveInstance[A](implicit @unused ev: Primitive[A]): Merge[A] = (a: A, _: A) => a

  inline def instance[A](using Mirror.Of[A]): Merge[A] = autoDerived[A]

  val ops: tofu.syntax.merge.type = tofu.syntax.merge

  sealed class Primitive[A]
  implicit object primitiveByte          extends Primitive[Byte]
  implicit object primitiveShort         extends Primitive[Short]
  implicit object primitiveInt           extends Primitive[Int]
  implicit object primitiveLong          extends Primitive[Long]
  implicit object primitiveChar          extends Primitive[Char]
  implicit object primitiveFloat         extends Primitive[Float]
  implicit object primitiveDouble        extends Primitive[Double]
  implicit object primitiveUnit          extends Primitive[Unit]
  implicit object primitiveBigDecimal    extends Primitive[BigDecimal]
  implicit object primitiveBigInt        extends Primitive[BigInt]
  implicit object primitiveLocalDateTime extends Primitive[LocalDateTime]
  implicit object primitiveZonedDateTime extends Primitive[ZonedDateTime]
  implicit object primitiveLocalDate     extends Primitive[LocalDate]
  implicit object primitiveInstant       extends Primitive[Instant]
  implicit object primitiveString        extends Primitive[String]
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
