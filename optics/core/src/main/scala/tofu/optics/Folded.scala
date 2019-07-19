package tofu.optics

import cats.data.Const
import cats.instances.list._
import cats.{Applicative, Foldable, Monoid}
import tofu.optics.data.Constant
import tofu.optics.data.Constant

/** S has some or none occurrences of A
  * and can collect them */
trait PFolded[-S, +T, +A, -B]  {
  def foldMap[X: Monoid](s: S)(f: A => X): X

  def getAll(s: S): List[A] = foldMap(s)(List(_))
}

object Folded extends MonoOpticCompanion(PFolded)

object PFolded extends OpticCompanion[PFolded] {
  def compose[S, T, A, B, U, V](f: PFolded[A, B, U, V], g: PFolded[S, T, A, B]): PFolded[S, T, U, V] =
    new PFolded[S, T, U, V] {
      def foldMap[X: Monoid](s: S)(fux: U => X): X = g.foldMap(s)(f.foldMap(_)(fux))
    }
  final implicit def byFoldable[F[_], A, T, B](implicit F: Foldable[F]): PFolded[F[A], T, A, B] =
    new PFolded[F[A], T, A, B] {
      def foldMap[X: Monoid](fa: F[A])(f: A => X): X = F.foldMap(fa)(f)
    }

  trait Context extends PReduced.Context with PItems.Context with PDowncast.Context {
    override def algebra: Monoid[X]
    def default: X = algebra.empty
    override val functor: Applicative[F] = {
      implicit val alg = algebra
      Applicative[Constant[X, +*]]
    }
  }

  override def toGeneric[S, T, A, B](o: PFolded[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => Constant[c.X, B]): S => Constant[c.X, T] =
        s => Constant.Impl(o.foldMap(s)(a => p(a).value)(c.algebra))
    }
  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PFolded[S, T, A, B] =
    new PFolded[S, T, A, B] {
      def foldMap[Y: Monoid](s: S)(f: A => Y): Y =
        o(new Context {
          type X = Y
          override def algebra = Monoid[Y]
        })(a => Constant(f(a)))(s).value
    }
}
