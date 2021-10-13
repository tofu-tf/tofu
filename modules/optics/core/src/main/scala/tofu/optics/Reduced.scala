package tofu.optics

import cats._
import cats.data.NonEmptyList
import tofu.optics.data._
import cats.syntax.semigroup._
import cats.instances.option._

/** aka NonEmptyFold S has some occurences of A and can collect then
  */
trait PReduced[-S, +T, +A, -B] extends PFolded[S, T, A, B] with PBase[PReduced, S, T, A, B] { self =>
  def reduceMap[X: Semigroup](s: S)(f: A => X): X

  override def foldMap[X: Monoid](s: S)(f: A => X): X = reduceMap(s)(f)
  def getAll1(s: S): NonEmptyList[A]                  = reduceMap(s)(NonEmptyList.one[A])

  def +++[S1 <: S, A1 >: A](other: PReduced[S1, Any, A1, Nothing]): PReduced[S1, Nothing, A1, Any] =
    new PReduced[S1, Nothing, A1, Any] {
      override def reduceMap[X: Semigroup](s: S1)(f: A1 => X): X =
        self.reduceMap(s)(f) |+| other.reduceMap(s)(f)
    }

  def :++[S1 <: S, A1 >: A](other: PFolded[S1, Any, A1, Nothing]): PReduced[S1, Nothing, A1, Any] =
    other match {
      case nonEmpty: PReduced[S1, Any, A1, Nothing] => +++(nonEmpty)
      case _                                        =>
        new PReduced[S1, Nothing, A1, Any] {
          override def reduceMap[X: Semigroup](s: S1)(f: A1 => X): X = {
            val res = self.reduceMap(s)(f)
            other.foldMap[Option[X]](s)(a => Some(f(a))).fold(res)(res |+| _)
          }
        }
    }

  def ++:[S1 <: S, A1 >: A](other: PFolded[S1, Any, A1, Nothing]): PReduced[S1, Nothing, A1, Any] =
    other match {
      case nonEmpty: PReduced[S1, Any, A1, Nothing] => nonEmpty +++ self
      case _                                        =>
        new PReduced[S1, Nothing, A1, Any] {
          override def reduceMap[X: Semigroup](s: S1)(f: A1 => X): X = {
            val res = self.reduceMap(s)(f)
            other.foldMap[Option[X]](s)(a => Some(f(a))).fold(res)(_ |+| res)
          }
        }
    }
}

object Reduced extends MonoOpticCompanion(PReduced) {
  def apply[S] = new ReducedApply[S]

  class ReducedApply[S] {
    type Arb

    def apply[A](fm: Semigroup[Arb] => (S, A => Arb) => Arb): Reduced[S, A] = new Reduced[S, A] {
      def reduceMap[X: Semigroup](s: S)(f: A => X): X =
        fm(Semigroup[X].asInstanceOf[Semigroup[Arb]])(s, f.asInstanceOf[A => Arb]).asInstanceOf[X]
    }
  }
}

object PReduced extends OpticCompanion[PReduced] {
  def compose[S, T, A, B, U, V](f: PReduced[A, B, U, V], g: PReduced[S, T, A, B]): PReduced[S, T, U, V] =
    new PComposed[PReduced, S, T, A, B, U, V](g, f) with PReduced[S, T, U, V] {
      def reduceMap[X: Semigroup](s: S)(fux: U => X): X = g.reduceMap(s)(f.reduceMap(_)(fux))
    }

  final implicit def byReducible[F[_], T, A, B](implicit F: Reducible[F]): PReduced[F[A], T, A, B] =
    new PReduced[F[A], T, A, B] {
      def reduceMap[X: Semigroup](fa: F[A])(f: A => X): X = F.reduceMap(fa)(f)
    }

  trait Context extends PRepeated.Context with PExtract.Context {
    def algebra: Semigroup[X]
    override def functor: Apply[Constant[X, *]] = {
      implicit val alg: Semigroup[X] = algebra
      Apply[Constant[X, *]]
    }
  }

  override def toGeneric[S, T, A, B](o: PReduced[S, T, A, B]): Optic[Context, S, T, A, B]   =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => Constant[c.X, B]): S => Constant[c.X, T] =
        s => o.reduceMap(s)(p)(c.algebra)
    }
  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PReduced[S, T, A, B] =
    new PReduced[S, T, A, B] {
      def reduceMap[Y: Semigroup](s: S)(f: A => Y): Y =
        o.apply(new Context {
          type X = Y
          def algebra = Semigroup[Y]
        })(f)(s)

    }

  override def delayed[S, T, A, B](o: () => PReduced[S, T, A, B]): PReduced[S, T, A, B] = new PReduced[S, T, A, B] {
    lazy val opt                                    = o()
    def reduceMap[X: Semigroup](s: S)(f: A => X): X = opt.reduceMap(s)(f)
  }
}
