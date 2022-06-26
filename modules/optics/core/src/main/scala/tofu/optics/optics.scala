package tofu.optics

import cats.arrow._
import tofu.optics.classes.Category2
import tofu.optics.tags.Tagger
import tofu.optics.classes.Delayed
import tofu.optics.compat.uv212

trait OpticCompanion[O[-s, +t, +a, -b] >: PSame[s, t, a, b] @uv212] {
  self =>
  type Context <: OpticContext
  type Mono[a, b] = O[a, a, b, b]

  def compose[S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B]): O[S, T, U, V]

  def toGeneric[S, T, A, B](o: O[S, T, A, B]): Optic[Context, S, T, A, B]
  def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): O[S, T, A, B]

  def delayed[S, T, A, B](o: () => O[S, T, A, B]): O[S, T, A, B]

  final implicit val category: Category[Mono] = new Category[Mono] {
    def id[A]                                                      = PSame.id[A, A]
    def compose[A, B, C](f: Mono[B, C], g: Mono[A, B]): Mono[A, C] = self.compose(f, g)
  }

  final implicit val delayed: Delayed[O] = new Delayed[O] {
    override def delayed[S, T, A, B](o: () => O[S, T, A, B]): O[S, T, A, B] = self.delayed(o)
  }

  final implicit val category2: Category2[O] = new Category2[O] {
    def id[A, B]: O[A, B, A, B]                                                      = PSame.id[A, B]
    def compose[S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B]): O[S, T, U, V] = self.compose(f, g)
  }

  final implicit def toOpticComposeOps[S, T, A, B](o: O[S, T, A, B]): OpticComposeOps[O, S, T, A, B] =
    new OpticComposeOps[O, S, T, A, B](o)

  final implicit def toMonoOpticOps[S, A](o: O[S, S, A, A]): MonoOpticOps[O, S, A] =
    new MonoOpticOps[O, S, A](o)

  final implicit def toDelayOps[S, T, A, B](o: => O[S, T, A, B]): DelayedOps[O, S, T, A, B] =
    new DelayedOps[O, S, T, A, B](() => o)

}

trait OpticProduct[O[s, t, a, b]] {
  def product[S1, S2, T1, T2, A1, A2, B1, B2](
      f: O[S1, T1, A1, B1],
      g: O[S2, T2, A2, B2]
  ): O[(S1, S2), (T1, T2), (A1, A2), (B1, B2)]
}

trait OpticContext {
  type F[+_]
  type P[-_, +_]
}

abstract class MonoOpticCompanion[PO[-s, +t, +a, -b] >: PSame[s, t, a, b] @uv212](
    poly: OpticCompanion[PO]
) {
  self =>
  type O[a, b] = PO[a, a, b, b]

  def apply[A, B](implicit o: O[A, B]): O[A, B] = o

  def compose[A, B, C](f: O[B, C], g: O[A, B]): O[A, C] = poly.compose(f, g)
}

class OpticComposeOps[O[s, t, a, b], S, T, A, B](private val o: O[S, T, A, B]) extends AnyVal {
  def to[O1[s, t, a, b] >: O[s, t, a, b]](
      tagger: Tagger[O1]
  ): WithTag[O1, S, T, A, B, tagger.Tag] =
    new WithTag(o)

  def >[O1[s, t, a, b] >: O[s, t, a, b]](
      tagger: Tagger[O1]
  ): WithTag[O1, S, T, A, B, tagger.Tag] =
    new WithTag(o)
}

class MonoOpticOps[O[s, t, a, b], S, A](private val o: O[S, S, A, A]) extends AnyVal {
  def ->:(s: S): Applied[O, S, S, A, A] = Applied(s, o)

  def applied_:(s: S): Applied[O, S, S, A, A] = Applied(s, o)
}

class DelayedOps[O[s, t, a, b], S, T, A, B](private val of: () => O[S, T, A, B]) extends AnyVal {
  def delayed(implicit d: Delayed[O]): O[S, T, A, B] = d.delayed(of)
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
