package tofu.optics

import cats._
import cats.data._
import cats.syntax.functor._
import tofu.optics.classes.PChoice
import tofu.optics.data._

/** aka Lens S has exactly one A and can update it
  */
trait PContains[-S, +T, +A, -B]
    extends PExtract[S, T, A, B] with PRepeated[S, T, A, B] with PProperty[S, T, A, B]
    with PBase[PContains, S, T, A, B] {
  def set(s: S, b: B): T
  def extract(s: S): A

  def get(s: S): A = extract(s)

  def narrow(s: S): Either[T, A]            = Right(extract(s))
  override def update(s: S, fab: A => B): T = set(s, fab(extract(s)))

  def project[F[+_]](s: S)(fab: A => F[B])(implicit F: Functor[F]): F[T] =
    F.map(fab(extract(s)))(set(s, _))

  override def reduceMap[X: Semigroup](s: S)(f: A => X): X             = f(extract(s))
  def traverse1[F[+_]](a: S)(f: A => F[B])(implicit F: Apply[F]): F[T] =
    F.map(f(extract(a)))(b => set(a, b))

  override def traverse[F[+_]](s: S)(f: A => F[B])(implicit F: Applicative[F]): F[T] = traverse1(s)(f)
  override def downcast(s: S): Option[A]                                             = Some(extract(s))
}

object Contains extends MonoOpticCompanion(PContains) {
  def apply[A] = new ContainsApplied[A](true)

  class ContainsApplied[A](private val dummy: Boolean) extends AnyVal {
    def apply[B](fget: A => B)(fset: (A, B) => A): Contains[A, B] = new Contains[A, B] {
      def set(s: A, b: B): A = fset(s, b)
      def extract(s: A): B   = fget(s)
    }
  }
}

object PContains extends OpticCompanion[PContains] with OpticProduct[PContains] {
  def apply[S, B] = new PContainsApplied[S, B](true)

  override def delayed[S, T, A, B](o: () => PContains[S, T, A, B]): PContains[S, T, A, B] = new PContains[S, T, A, B] {
    lazy val opt = o()

    override def set(s: S, b: B): T = opt.set(s, b)

    override def extract(s: S): A = opt.extract(s)
  }
  class PContainsApplied[S, B](private val dummy: Boolean) extends AnyVal {
    def apply[A, T](fget: S => A)(fset: (S, B) => T): PContains[S, T, A, B] = new PContains[S, T, A, B] {
      def set(s: S, b: B): T = fset(s, b)
      def extract(s: S): A   = fget(s)
    }

    def apply[A, T](name: String)(fget: S => A)(fset: (S, B) => T): PContains[S, T, A, B] = new PContains[S, T, A, B] {
      def set(s: S, b: B): T = fset(s, b)
      def extract(s: S): A   = fget(s)

      override def toString(): String = name
    }
  }

  def compose[S, T, A, B, U, V](f: PContains[A, B, U, V], g: PContains[S, T, A, B]): PContains[S, T, U, V] =
    new PComposed[PContains, S, T, A, B, U, V](g, f) with PContains[S, T, U, V] {
      def set(s: S, b: V): T = g.set(s, f.set(g.extract(s), b))
      def extract(a: S): U   = f.extract(g.extract(a))
    }

  override def product[S1, S2, T1, T2, A1, A2, B1, B2](
      f: PContains[S1, T1, A1, B1],
      g: PContains[S2, T2, A2, B2]
  ): PContains[(S1, S2), (T1, T2), (A1, A2), (B1, B2)] = new PContains[(S1, S2), (T1, T2), (A1, A2), (B1, B2)] {
    override def set(s: (S1, S2), b: (B1, B2)): (T1, T2) = (f.set(s._1, b._1), g.set(s._2, b._2))

    override def extract(s: (S1, S2)): (A1, A2) = (f.extract(s._1), g.extract(s._2))
  }

  trait ByProject[S, T, A, B] extends PContains[S, T, A, B] {
    def proj[F[+_]: Functor](a: S, fb: A => F[B]): F[T]
    override def project[F[+_]: Functor](a: S)(fb: A => F[B]) = proj[F](a, fb)

    def set(a: S, b: B): T = proj[Identity](a, _ => b)
    def extract(a: S): A   = proj[Constant[A, +*]](a, identity)(constantFunctor)
  }

  trait Context extends PEquivalent.Context {
    override type P[-x, +y] = x => y
    override def profunctor: PChoice[P] = PChoice[P]
  }

  override def toGeneric[S, T, A, B](o: PContains[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => c.F[B]): S => c.F[T] = a => o.project(a)(p)(c.functor)
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PContains[S, T, A, B] =
    new ByProject[S, T, A, B] {
      def proj[G[+_]: Functor](a: S, fb: A => G[B]): G[T] =
        o(new Context {
          type F[+x] = G[x]
          def functor = Functor[G]
        })(fb)(a)
    }

  implicit final class PContainsOps[S, T, A, B](private val self: PContains[S, T, A, B]) extends AnyVal {
    def focusState[F[+_]: Functor, R](state: IndexedStateT[F, A, B, R]): IndexedStateT[F, S, T, R] =
      IndexedStateT.applyF[F, S, T, R](
        state.runF.map(afbr => (s: S) => afbr(self.extract(s)).map { case (b, r) => (self.set(s, b), r) })
      )
  }
}
