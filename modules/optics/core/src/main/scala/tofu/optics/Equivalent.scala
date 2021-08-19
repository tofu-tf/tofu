package tofu.optics

import cats._
import cats.arrow._
import cats.instances.function._
import tofu.optics.data._

/** aka Iso S and B share same information
  */
trait PEquivalent[-S, +T, +A, -B]
    extends PSubset[S, T, A, B] with PContains[S, T, A, B] with PBase[PEquivalent, S, T, A, B]
    with PZipping[S, T, A, B] {
  self =>
  def extract(s: S): A
  def back(b: B): T

  def inverse(b: B): T = upcast(b)

  override def upcast(b: B): T = back(b)

  def grate(f: (S => A) => B): T = back(f(extract))

  override def update(s: S, ab: A => B): T = back(ab(extract(s)))

  override def traverse[F[+_]](a: S)(f: A => F[B])(implicit F: Applicative[F]): F[T] =
    F.map(f(extract(a)))(back)

  def employ[F[+_], P[-_, +_]](pb: P[A, F[B]])(implicit F: Functor[F], P: Profunctor[P]): P[S, F[T]] =
    P.dimap(pb)(extract)(F.map(_)(back))

  override def downcast(s: S): Option[A] = Some(extract(s))

  def inverse: PEquivalent[B, A, T, S] = new PEquivalent[B, A, T, S] {
    def back(s: S): A    = self.extract(s)
    def extract(b: B): T = self.back(b)
  }
}

object Equivalent extends MonoOpticCompanion(PEquivalent) {
  def apply[A] = new EquivalentApplied[A](true)

  class EquivalentApplied[A](private val dummy: Boolean) extends AnyVal {
    def apply[B](fget: A => B)(finv: B => A): Equivalent[A, B] = new Equivalent[A, B] {
      def extract(s: A): B = fget(s)
      def back(b: B): A    = finv(b)
    }

    def apply[B](name: String)(fget: A => B)(finv: B => A): Equivalent[A, B] = new Equivalent[A, B] {
      def extract(s: A): B = fget(s)
      def back(b: B): A    = finv(b)

      override def toString(): String = name
    }
  }
}

object PEquivalent extends OpticCompanion[PEquivalent] with OpticProduct[PEquivalent] {
  def apply[S, B] = new PEquivalentApplied[S, B](true)

  class PEquivalentApplied[S, B](private val dummy: Boolean) extends AnyVal {
    def apply[T, A](fget: S => A)(finv: B => T): PEquivalent[S, T, A, B] = new PEquivalent[S, T, A, B] {
      def extract(s: S): A = fget(s)
      def back(b: B): T    = finv(b)
    }

    def apply[T, A](name: String)(fget: S => A)(finv: B => T): PEquivalent[S, T, A, B] = new PEquivalent[S, T, A, B] {
      def extract(s: S): A = fget(s)
      def back(b: B): T    = finv(b)

      override def toString(): String = name
    }
  }

  def compose[S, T, A, B, U, V](f: PEquivalent[A, B, U, V], g: PEquivalent[S, T, A, B]): PEquivalent[S, T, U, V] =
    new PComposed[PEquivalent, S, T, A, B, U, V](g, f) with PEquivalent[S, T, U, V] {
      def extract(s: S): U = f.extract(g.extract(s))
      def back(v: V): T    = g.upcast(f.upcast(v))
    }

  override def product[S1, S2, T1, T2, A1, A2, B1, B2](
      f: PEquivalent[S1, T1, A1, B1],
      g: PEquivalent[S2, T2, A2, B2]
  ): PEquivalent[(S1, S2), (T1, T2), (A1, A2), (B1, B2)] = new PEquivalent[(S1, S2), (T1, T2), (A1, A2), (B1, B2)] {
    override def extract(s: (S1, S2)): (A1, A2) = (f.extract(s._1), g.extract(s._2))
    override def back(b: (B1, B2)): (T1, T2)    = (f.upcast(b._1), g.upcast(b._2))
  }

  trait ByEmploy[S, T, A, B] extends PEquivalent[S, T, A, B] {
    def emp[F[+_]: Functor, P[-_, +_]: Profunctor](pb: P[A, F[B]]): P[S, F[T]]
    override def employ[F[+_]: Functor, P[-_, +_]: Profunctor](pb: P[A, F[B]]): P[S, F[T]] = emp(pb)

    def extract(a: S): A = employ[Constant[A, +*], -* => +*](identity).apply(a)
    def back(b: B): T    = employ[B => +*, Tagged](_ => b => b).apply(())(b)
  }

  trait Context extends OpticContext {
    def functor: Functor[this.F]
    def profunctor: Profunctor[P]
  }

  override def toGeneric[S, T, A, B](o: PEquivalent[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: c.P[A, c.F[B]]): c.P[S, c.F[T]] =
        o.employ[c.F, c.P](p)(c.functor, c.profunctor)
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PEquivalent[S, T, A, B] =
    new ByEmploy[S, T, A, B] {
      def emp[G[+_]: Functor, Q[-_, +_]: Profunctor](pb: Q[A, G[B]]): Q[S, G[T]] =
        o(new Context {
          def functor    = Functor[F]
          def profunctor = Profunctor[Q]
          type F[+x]     = G[x]
          type P[-x, +y] = Q[x, y]
        })(pb)
    }

  override def delayed[S, T, A, B](o: () => PEquivalent[S, T, A, B]): PEquivalent[S, T, A, B] =
    new PEquivalent[S, T, A, B] {
      lazy val opt         = o()
      def extract(s: S): A = opt.extract(s)

      def back(b: B): T = opt.upcast(b)
    }
}
