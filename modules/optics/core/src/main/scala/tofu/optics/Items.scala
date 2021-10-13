package tofu.optics

import alleycats.Pure
import cats._
import tofu.optics.data._

/** aka Traversal S has some or none occurences of A and can update them using some effect
  */
trait PItems[-S, +T, +A, -B] extends PUpdate[S, T, A, B] with PFolded[S, T, A, B] with PBase[PItems, S, T, A, B] {
  def traverse[F[+_]: Applicative](s: S)(f: A => F[B]): F[T]

  def update(a: S, fb: A => B): T            = traverse[Identity](a)(fb)
  def foldMap[X: Monoid](a: S)(f: A => X): X = traverse[Constant[X, +*]](a)(f)
}

object Items extends MonoOpticCompanion(PItems) {
  def apply[S] = new ItemsApply[S]

  class ItemsApply[S] {
    type Arb[+_]

    def apply[A](trav: Applicative[Arb] => (S, A => Arb[A]) => Arb[S]): Items[S, A] = new Items[S, A] {
      def traverse[F[+_]: Applicative](s: S)(f: A => F[A]): F[S] =
        trav(Applicative[F].asInstanceOf[Applicative[Arb]])(s, f.asInstanceOf[A => Arb[A]]).asInstanceOf[F[S]]
    }
  }
}

object PItems extends OpticCompanion[PItems] {
  def apply[S, A, B] = new PItemsApply[S, A, B]

  class PItemsApply[S, A, B] {
    type Arb[+_]

    def apply[T](trav: Applicative[Arb] => (S, A => Arb[B]) => Arb[T]): PItems[S, T, A, B] = new PItems[S, T, A, B] {
      def traverse[F[+_]: Applicative](s: S)(f: A => F[B]): F[T] =
        trav(Applicative[F].asInstanceOf[Applicative[Arb]])(s, f.asInstanceOf[A => Arb[B]]).asInstanceOf[F[T]]
    }
  }

  def compose[S, T, A, B, U, V](f: PItems[A, B, U, V], g: PItems[S, T, A, B]): PItems[S, T, U, V] =
    new PComposed[PItems, S, T, A, B, U, V](g, f) with PItems[S, T, U, V] {
      def traverse[F[+_]: Applicative](a: S)(fc: U => F[V]): F[T] = g.traverse(a)(f.traverse(_)(fc))
    }

  final implicit def fromTraverse[F[+_], A, B](implicit F: Traverse[F]): PItems[F[A], F[B], A, B] =
    new PItems[F[A], F[B], A, B] {
      def traverse[G[_]: Applicative](a: F[A])(f: A => G[B]): G[F[B]] = F.traverse(a)(f)

      override def foldMap[X: Monoid](a: F[A])(f: A => X): X = F.foldMap(a)(f)

      override def getAll(s: F[A]): List[A] = F.toList(s)

      override def update(a: F[A], fb: A => B): F[B] = F.map(a)(fb)

      override def put(s: F[A], b: B): F[B] = F.as(s, b)
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

  override def delayed[S, T, A, B](o: () => PItems[S, T, A, B]): PItems[S, T, A, B] = new PItems[S, T, A, B] {
    val opt                                                    = o()
    def traverse[F[+_]: Applicative](s: S)(f: A => F[B]): F[T] = opt.traverse(s)(f)
  }
}
