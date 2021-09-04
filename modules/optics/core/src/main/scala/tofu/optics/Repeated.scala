package tofu.optics

import cats._
import tofu.optics.data._

/** aka NonEmptyTraversal S has some occurrences of A and can update them using some effect
  */
trait PRepeated[-S, +T, +A, -B] extends PItems[S, T, A, B] with PReduced[S, T, A, B] with PBase[PRepeated, S, T, A, B] {
  def traverse1[F[+_]: Apply](s: S)(f: A => F[B]): F[T]

  def traverse[F[+_]: Applicative](s: S)(f: A => F[B]): F[T] = traverse1[F](s)(f)
  override def foldMap[X: Monoid](s: S)(f: A => X): X        = reduceMap[X](s)(f)
  def reduceMap[X: Semigroup](s: S)(f: A => X): X            = traverse1[Constant[X, +*]](s)(f)
}

object Repeated extends MonoOpticCompanion(PRepeated) {
  def apply[S] = new RepeatedApply[S]

  class RepeatedApply[S] {
    type Arb[+_]

    def apply[A](trav: Apply[Arb] => (S, A => Arb[A]) => Arb[S]): Repeated[S, A] = new Repeated[S, A] {
      def traverse1[F[+_]: Apply](s: S)(f: A => F[A]): F[S] =
        trav(Apply[F].asInstanceOf[Apply[Arb]])(s, f.asInstanceOf[A => Arb[A]]).asInstanceOf[F[S]]
    }
  }
}

object PRepeated extends OpticCompanion[PRepeated] {
  def compose[S, T, A, B, U, V](f: PRepeated[A, B, U, V], g: PRepeated[S, T, A, B]): PRepeated[S, T, U, V]    =
    new PComposed[PRepeated, S, T, A, B, U, V](g, f) with PRepeated[S, T, U, V] {
      def traverse1[F[+_]: Apply](s: S)(fuv: U => F[V]): F[T] = g.traverse1(s)(f.traverse1(_)(fuv))
    }
  final implicit def fromNETraverse[F[_], A, B](implicit F: NonEmptyTraverse[F]): PRepeated[F[A], F[B], A, B] =
    new PRepeated[F[A], F[B], A, B] {
      def traverse1[G[+_]: Apply](a: F[A])(f: A => G[B]): G[F[B]] = F.nonEmptyTraverse(a)(f)
    }

  trait Context extends PContains.Context {
    def functor: Apply[F]
  }

  override def toGeneric[S, T, A, B](o: PRepeated[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => c.F[B]): S => c.F[T] = a => o.traverse1(a)(p)(c.functor)
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PRepeated[S, T, A, B] =
    new PRepeated[S, T, A, B] {
      def traverse1[G[+_]: Apply](s: S)(f: A => G[B]): G[T] =
        o.apply(new Context {
          def functor = Apply[G]
          type F[+x] = G[x]
        })(f)(s)
    }

  def toMono[A, B](o: PRepeated[A, A, B, B]): Repeated[A, B] = new Repeated[A, B] {
    def traverse1[F[+_]: Apply](a: A)(f: B => F[B]): F[A] = o.traverse1(a)(f)
  }

  override def delayed[S, T, A, B](o: () => PRepeated[S, T, A, B]): PRepeated[S, T, A, B] = new PRepeated[S, T, A, B] {
    lazy val opt                                          = o()
    def traverse1[F[+_]: Apply](s: S)(f: A => F[B]): F[T] = opt.traverse1(s)(f)
  }
}
