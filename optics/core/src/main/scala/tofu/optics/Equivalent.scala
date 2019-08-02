package tofu.optics

import cats._
import cats.arrow._
import cats.instances.function._
import tofu.optics.data.{Constant, Tagged}

/** aka Iso
  * S and B share same information
  */
trait PEquivalent[-S, +T, +A, -B] extends PSubset[S, T, A, B] with PContains[S, T, A, B] {
  self =>
  def extract(s: S): A
  def upcast(b: B): T

  def inverse(b: B): T = upcast(b)

  override def traverse[F[+_]](a: S)(f: A => F[B])(implicit F: Applicative[F]): F[T] =
    F.map(f(extract(a)))(upcast)

  def employ[F[+_], P[-_, +_]](pb: P[A, F[B]])(implicit F: Functor[F], P: Profunctor[P]): P[S, F[T]] =
    P.dimap(pb)(extract)(F.map(_)(upcast))

  override def downcast(s: S): Option[A] = Some(extract(s))

  def inverse: PEquivalent[B, A, T, S] = new PEquivalent[B, A, T, S] {
    def upcast(s: S): A  = self.extract(s)
    def extract(b: B): T = self.upcast(b)
  }
}

object Equivalent extends MonoOpticCompanion(PEquivalent) {
  def apply[A] = new EquivalentApplied[A](true)

  class EquivalentApplied[A](private val dummy: Boolean) extends AnyVal {
    def apply[B](fget: A => B)(finv: B => A): Equivalent[A, B] = new Equivalent[A, B] {
      def extract(s: A): B = fget(s)
      def upcast(b: B): A  = finv(b)
    }
  }
}

object PEquivalent extends OpticCompanion[PEquivalent] {
  def apply[S, B] = new PEquivalentApplied[S, B](true)

  class PEquivalentApplied[S, B](private val dummy: Boolean) extends AnyVal {
    def apply[T, A](fget: S => A)(finv: B => T): PEquivalent[S, T, A, B] = new PEquivalent[S, T, A, B] {
      def extract(s: S): A = fget(s)
      def upcast(b: B): T  = finv(b)
    }
  }

  def compose[S, T, A, B, U, V](f: PEquivalent[A, B, U, V], g: PEquivalent[S, T, A, B]): PEquivalent[S, T, U, V] =
    new PEquivalent[S, T, U, V] {
      def extract(s: S): U = f.extract(g.extract(s))
      def upcast(v: V): T  = g.upcast(f.upcast(v))
    }

  trait ByEmploy[S, T, A, B] extends PEquivalent[S, T, A, B] {
    def emp[F[+_]: Functor, P[-_, +_]: Profunctor](pb: P[A, F[B]]): P[S, F[T]]
    override def employ[F[+_]: Functor, P[-_, +_]: Profunctor](pb: P[A, F[B]]): P[S, F[T]] = emp(pb)

    def extract(a: S): A = employ[Constant[A, +*], -* => +*](b => Constant.Impl(b)).apply(a).value
    def upcast(b: B): T  = employ[B => +*, Tagged](Tagged(b => b)).value(b)
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
}
