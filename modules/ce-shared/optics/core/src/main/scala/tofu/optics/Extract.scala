package tofu.optics

import cats.{Functor, Monoid, Semigroup}
import tofu.optics.data._

/** aka Getter A has exactly one B mere function from A to B and part of Lens
  */
trait PExtract[-S, +T, +A, -B]
    extends PDowncast[S, T, A, B] with PReduced[S, T, A, B] with PBase[PExtract, S, T, A, B] {
  def extract(s: S): A

  override def as[B1, T1]: PExtract[S, T1, A, B1] = this.asInstanceOf[PExtract[S, T1, A, B1]]

  def downcast(s: S): Option[A]                       = Some(extract(s))
  def reduceMap[X: Semigroup](s: S)(f: A => X): X     = f(extract(s))
  override def foldMap[X: Monoid](s: S)(f: A => X): X = f(extract(s))
}

object Extract extends MonoOpticCompanion(PExtract)

object PExtract extends OpticCompanion[PExtract] {
  def compose[S, T, A, B, U, V](f: PExtract[A, B, U, V], g: PExtract[S, T, A, B]): PExtract[S, T, U, V] =
    new PComposed[PExtract, S, T, A, B, U, V](g, f) with PExtract[S, T, U, V] {
      def extract(s: S): U = f.extract(g.extract(s))
    }

  trait Context extends PContains.Context {
    type X
    type F[+A] = Constant[X, A]
    def functor: Functor[Constant[X, *]] = implicitly
  }
  override def toGeneric[S, T, A, B](o: PExtract[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => Constant[c.X, B]): S => Constant[c.X, T] = s => p(o.extract(s))
    }
  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PExtract[S, T, A, B] =
    s => o(new Context { type X = A })(identity)(s)

  override def delayed[S, T, A, B](o: () => PExtract[S, T, A, B]): PExtract[S, T, A, B] = new PExtract[S, T, A, B] {
    lazy val opt         = o()
    def extract(s: S): A = opt.extract(s)
  }
}
