package tofu.optics

import alleycats.Pure
import cats._
import cats.instances.either._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.profunctor._
import tofu.optics.Subset.ByDowncast
import tofu.optics.data.Identity
import tofu.optics.classes.PChoice
import tofu.optics.data._

import scala.reflect.ClassTag

/** aka Prism any of S could be equivalent to A
  */
trait PSubset[-S, +T, +A, -B]
    extends PUpcast[S, T, A, B] with PDowncast[S, T, A, B] with PItems[S, T, A, B] with PProperty[S, T, A, B]
    with PBase[PSubset, S, T, A, B] { self =>
  def narrow(s: S): Either[T, A]

  def set(s: S, b: B): T                                                                                       = upcast(b)
  def inject[F[+_], P[-_, +_]](pb: P[A, F[B]])(implicit FP: Pure[F], F: Functor[F], P: PChoice[P]): P[S, F[T]] =
    P.right[A, F[B], T](pb).dimap[S, F[T]](narrow)(_.fold[F[T]](FP.pure, F.map(_)(upcast)))
}

object Subset extends MonoOpticCompanion(PSubset) {
  def apply[A] = new SubsetApplied[A](true)

  class SubsetApplied[A](private val dummy: Boolean) extends AnyVal {
    def apply[B](fdown: A => Option[B])(fup: B => A): Subset[A, B] = new ByDowncast[A, B] {
      def cast(a: A): Option[B] = fdown(a)
      def upcast(b: B): A       = fup(b)
    }
  }

  trait ByDowncast[A, B] extends Subset[A, B] {
    def cast(a: A): Option[B]

    def narrow(s: A): Either[A, B]         = Either.fromOption(cast(s), s)
    override def downcast(s: A): Option[B] = cast(s)
  }
}

object PSubset extends OpticCompanion[PSubset] {
  override type Mono[A, B] = Subset[A, B]

  def apply[S, B] = new SubsetApplied[S, B](true)

  class SubsetApplied[S, B](private val dummy: Boolean) extends AnyVal {
    def apply[T, A](fdown: S => Either[T, A])(fup: B => T): PSubset[S, T, A, B]               = new PSubset[S, T, A, B] {
      def narrow(s: S): Either[T, A] = fdown(s)
      def upcast(b: B): T            = fup(b)
    }
    def apply[T, A](name: String)(fdown: S => Either[T, A])(fup: B => T): PSubset[S, T, A, B] =
      new PSubset[S, T, A, B] {
        def narrow(s: S): Either[T, A]  = fdown(s)
        def upcast(b: B): T             = fup(b)
        override def toString(): String = name
      }
  }

  def compose[S, T, A, B, U, V](f: PSubset[A, B, U, V], g: PSubset[S, T, A, B]): PSubset[S, T, U, V] =
    new PComposed[PSubset, S, T, A, B, U, V](g, f) with PSubset[S, T, U, V] {
      def narrow(s: S): Either[T, U] = g.narrow(s).flatMap(f.narrow(_).leftMap(g.upcast))
      def upcast(v: V): T            = g.upcast(f.upcast(v))
    }

  trait ByInject[S, T, A, B] extends PSubset[S, T, A, B] {
    implicit val boptMonoid: Monoid[Option[A]] = MonoidK[Option].algebra[A]

    def inj[F[+_]: Pure: Functor, P[-_, +_]: PChoice](pb: P[A, F[B]]): P[S, F[T]]
    override def inject[F[+_]: Pure: Functor, P[-_, +_]: PChoice](pb: P[A, F[B]]): P[S, F[T]] = inj(pb)

    def upcast(b: B): T            = inj[Identity, Tagged](_ => b).apply(())
    def narrow(s: S): Either[T, A] = inj[Either[A, +*], Function](Left(_)).apply(s).swap
  }

  trait Context extends PEquivalent.Context { ctx =>
    def pure: Pure[F]
    def profunctor: PChoice[P]
  }

  override def toGeneric[S, T, A, B](o: PSubset[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: c.P[A, c.F[B]]): c.P[S, c.F[T]] =
        o.inject[c.F, c.P](p)(c.pure, c.functor, c.profunctor)
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PSubset[S, T, A, B] = new ByInject[S, T, A, B] {
    def inj[G[+_]: Pure: Functor, Q[-_, +_]: PChoice](pb: Q[A, G[B]]): Q[S, G[T]] =
      o.apply(new Context {
        def functor    = Functor[G]
        def profunctor = PChoice[Q]
        def pure       = Pure[F]
        type F[+x]     = G[x]
        type P[-x, +y] = Q[x, y]
      })(pb)
  }

  implicit def subType[A, B <: A: ClassTag]: Subset[A, B] = new ByDowncast[A, B] {
    def cast(a: A): Option[B] = Some(a).collect { case b: B => b }
    def upcast(b: B): A       = b
  }

  override def delayed[S, T, A, B](o: () => PSubset[S, T, A, B]): PSubset[S, T, A, B] = new PSubset[S, T, A, B] {
    lazy val opt        = o()
    def upcast(b: B): T = opt.upcast(b)

    def narrow(s: S): Either[T, A] = opt.narrow(s)

  }
}
