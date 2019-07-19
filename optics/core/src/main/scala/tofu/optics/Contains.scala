package tofu.optics

import cats._
import cats.data._
import cats.syntax.functor._
import cats.syntax.bifunctor._
import cats.instances.tuple._
import tofu.optics.classes.PChoice
import tofu.optics.data.{Constant, Identity}
import tofu.optics.classes.PChoice

import scala.annotation.unchecked.uncheckedVariance

/** aka Lens
  * S has exactly one A and can update it
  */
trait PContains[-S, +T, +A, -B] extends PExtract[S, T, A, B] with PRepeated[S, T, A, B] with PProperty[S, T, A, B] {
  def set(s: S, b: B): T
  def extract(s: S): A

  def narrow(s: S): Either[T, A]            = Right(extract(s))
  override def update(s: S, fab: A => B): T = set(s, fab(extract(s)))

  def project[F[+_]](s: S)(fab: A => F[B])(implicit F: Functor[F]): F[T] =
    F.map(fab(extract(s)))(set(s, _))

  override def reduceMap[X: Semigroup](s: S)(f: A => X): X = f(extract(s))
  def traverse1[F[+_]](a: S)(f: A => F[B])(implicit F: Apply[F]): F[T] =
    F.map(f(extract(a)))(b => set(a, b))

  override def traverse[F[+_]](s: S)(f: A => F[B])(implicit F: Applicative[F]): F[T] = traverse1(s)(f)
  override def downcast(s: S): Option[A]                                            = Some(extract(s))
}

object Contains extends MonoOpticCompanion(PContains)

object PContains extends OpticCompanion[PContains] {
  def compose[S, T, A, B, U, V](f: PContains[A, B, U, V], g: PContains[S, T, A, B]): PContains[S, T, U, V] =
    new PContains[S, T, U, V] {
      def set(s: S, b: V): T = g.set(s, f.set(g.extract(s), b))
      def extract(a: S): U   = f.extract(g.extract(a))
    }

  trait ByProject[S, T, A, B] extends PContains[S, T, A, B] {
    def proj[F[+_]: Functor](a: S, fb: A => F[B]): F[T]
    override def project[F[+_]: Functor](a: S)(fb: A => F[B]) = proj[F](a, fb)

    def set(a: S, b: B): T = proj[Identity](a, _ => b)
    def extract(a: S): A   = proj(a, Constant[A]).value
  }

  trait Context extends PEquivalent.Context {
    type P[-x, +y] = x => y
    val profunctor: PChoice[P] = PChoice[P]
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

  implicit class PContainsOps[S, T, A, B](private val self: PContains[S, T, A, B]) extends AnyVal {
    def focusState[F[+ _]: Functor, R](state: IndexedStateT[F, A, B, R]): IndexedStateT[F, S, T, R] =
      IndexedStateT.applyF(state.runF.map(afbr => (s: S) => afbr(self.extract(s)).map(_.leftMap(self.set(s, _)))))
  }

}
