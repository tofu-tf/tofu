package tofu.optics

import cats.arrow._
import tofu.optics.classes.Category2

trait OpticCompanion[O[s, t, a, b] >: PSame[s, t, a, b]] {
  self =>
  type Context <: OpticContext
  type Mono[a, b] = O[a, a, b, b]

  def compose[S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B]): O[S, T, U, V]

  def toGeneric[S, T, A, B](o: O[S, T, A, B]): Optic[Context, S, T, A, B]
  def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): O[S, T, A, B]

  final implicit val category: Category[Mono] = new Category[Mono] {
    def id[A]                                                      = PSame.id[A, A]
    def compose[A, B, C](f: Mono[B, C], g: Mono[A, B]): Mono[A, C] = self.compose(f, g)
  }

  final implicit val category2: Category2[O] = new Category2[O] {
    def id[A, B]: O[A, B, A, B]                                                      = PSame.id[A, B]
    def compose[S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B]): O[S, T, U, V] = self.compose(f, g)
  }

  final implicit def toOpticComposeOps[S, T, A, B](o: O[S, T, A, B]) = new OpticComposeOps[O, S, T, A, B](o)

  final implicit def toMonoOpticOps[S, A](o: O[S, S, A, A]) = new MonoOpticOps[O, S, A](o)

}

trait OpticContext {
  type F[+_]
  type P[-_, +_]
}

abstract class MonoOpticCompanion[PO[s, t, a, b] >: PSame[s, t, a, b]](
    poly: OpticCompanion[PO]
) {
  self =>
  type O[a, b] = PO[a, a, b, b]

  def apply[A, B](implicit o: O[A, B]): O[A, B] = o

  def compose[A, B, C](f: O[B, C], g: O[A, B]): O[A, C] = poly.compose(f, g)
}

class OpticComposeOps[O[s, t, a, b], S, T, A, B](private val o: O[S, T, A, B]) extends AnyVal {
  def andThen[O1[s, t, a, b] >: O[s, t, a, b], U, V](o1: O1[A, B, U, V])(
      implicit category2: Category2[O1]
  ): O1[S, T, U, V] = category2.compose(o1, o)

  def >>[O1[s, t, a, b] >: O[s, t, a, b]: Category2, U, V](o1: O1[A, B, U, V]): O1[S, T, U, V] = andThen(o1)
}

class MonoOpticOps[O[s, t, a, b], S, A](private val o: O[S, S, A, A]) extends AnyVal {
  def ->:(s: S): Applied[O, S, S, A, A] = Applied(s, o)
}

/** generic optic form, aka Profunctor Optic */
trait Optic[-Ctx <: OpticContext, -S, +T, +A, -B] {
  self =>
  def apply(c: Ctx)(p: c.P[A, c.F[B]]): c.P[S, c.F[T]]

  def andThen[C1 <: Ctx, U, V](that: Optic[C1, A, B, U, V]): Optic[C1, S, T, U, V] =
    new Optic[C1, S, T, U, V] {
      def apply(c: C1)(p: c.P[U, c.F[V]]): c.P[S, c.F[T]] = self(c)(that(c)(p))
    }
}

object Optic {
  type Mono[C <: OpticContext, A, B] = Optic[C, A, A, B, B]
  def id[Ctx <: OpticContext, S, A]: Optic[Ctx, S, A, S, A] =
    PSame.toGeneric(PSame.id)

  implicit def opticCategoryInstance[Ctx <: OpticContext]: Category[Mono[Ctx, *, *]] =
    new Category[Mono[Ctx, *, *]] {
      def id[A]: Mono[Ctx, A, A]                                                    = Optic.id
      def compose[A, B, C](f: Mono[Ctx, B, C], g: Mono[Ctx, A, B]): Mono[Ctx, A, C] = g.andThen(f)
    }
}
