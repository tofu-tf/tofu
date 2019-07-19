package tofu.optics

import alleycats.Pure
import cats._
import cats.data.Const
import tofu.optics.data.{Constant, Identity}

/** aka Traversal
  * S has some or none occurences of A
  * and can update them using some effect
  */
trait PItems[-S, +T, +A, -B] extends PUpdate[S, T, A, B] with PFolded[S, T, A, B] {
  def traverse[F[+_]: Applicative](a: S)(f: A => F[B]): F[T]

  def update(a: S, fb: A => B): T            = traverse[Identity](a)(fb)
  def foldMap[X: Monoid](a: S)(f: A => X): X = traverse[Constant[X, +*]](a)(b => Constant(f(b))).value
}

object Items extends MonoOpticCompanion(PItems)

object PItems extends OpticCompanion[PItems] {
  def compose[S, T, A, B, U, V](f: PItems[A, B, U, V], g: PItems[S, T, A, B]): PItems[S, T, U, V] =
    new PItems[S, T, U, V] {
      def traverse[F[+ _]: Applicative](a: S)(fc: U => F[V]): F[T] = g.traverse(a)(f.traverse(_)(fc))
    }

  final implicit def fromTraverse[F[+ _], A, B](implicit F: Traverse[F]): PItems[F[A], F[B], A, B] =
    new PItems[F[A], F[B], A, B] {
      def traverse[G[_]: Applicative](a: F[A])(f: A => G[B]): G[F[B]] = F.traverse(a)(f)
    }

  trait Context extends PSubset.Context with PRepeated.Context {
    def functor: Applicative[F]
    def pure = {
      implicit val f: Applicative[F] = functor
      Pure[F]
    }
  }

  override def toGeneric[S, T, A, B](o: PItems[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => c.F[B]): S => c.F[T] = a => o.traverse(a)(p)(c.functor)
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PItems[S, T, A, B] = new PItems[S, T, A, B] {
    def traverse[G[+_]: Applicative](a: S)(f: A => G[B]): G[T] =
      o.apply(new Context {
        def functor = Applicative[G]
        type F[+x] = G[x]
      })(f)(a)
  }
}
