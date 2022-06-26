package tofu.optics

import cats.Functor
import cats.syntax.functor._
import tofu.optics.data.Identity
import tofu.optics.data.CoKleisli
import cats.arrow.Profunctor
import cats.instances.function._
import cats.Applicative
import tofu.optics.compat.uv212

/** aka Grate see https://r6research.livejournal.com/28050.html
  */
trait PZipping[-S, +T, +A, -B] extends PBase[PZipping, S, T, A, B] with PUpdate[S, T, A, B] with PUpcast[S, T, A, B] {
  def grate(sab: (S => A) => B): T

  def update(s: S, ab: A => B): T = grate(sa => ab(sa(s)))

  def combineWith[F[+_]: Functor](f: F[A] => B)(fs: F[S]): T =
    grate(sa => f(fs.map(sa)))

  def zipWith(f: (A, A) => B)(s1: S, s2: S): T = grate(sa => f(sa(s1), sa(s2)))

  def upcast(b: B): T = grate(_ => b)
}

object PZipping extends OpticCompanion[PZipping] {
  override def compose[S, T, A, B, U, V](f: PZipping[A, B, U, V], g: PZipping[S, T, A, B]): PZipping[S, T, U, V] =
    new PComposed[PZipping, S, T, A, B, U, V](g, f) with PZipping[S, T, U, V] {
      def grate(suv: (S => U) => V): T = g.grate(sa => f.grate(au => suv(au compose sa)))
    }

  override def delayed[S, T, A, B](o: () => PZipping[S, T, A, B]): PZipping[S, T, A, B] = new PZipping[S, T, A, B] {
    lazy val opt                     = o()
    def grate(sab: (S => A) => B): T = opt.grate(sab)
  }

  trait Context extends PEquivalent.Context {
    type G[+a]
    override type F[+a] = a
    type P[-a, +b]      = CoKleisli[G, a, b]
    override def functor: Applicative[F @uv212] = Applicative[Identity @uv212]
    implicit def gfunctor: Functor[G]

    def profunctor: Profunctor[P] = new CoKleisliProfunctor[G]
  }

  override def toGeneric[S, T, A, B](o: PZipping[S, T, A, B]) = new Optic[Context, S, T, A, B] {
    def apply(c: Context)(p: c.G[A] => B): c.G[S] => T =
      o.combineWith(p)(_)(c.gfunctor)
  }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]) = new PZipping[S, T, A, B] {
    def grate(sab: (S => A) => B): T = o(new Context {
      type G[+a] = S => a
      def gfunctor: Functor[G] = implicitly
    })(sab)(identity)
  }
}

object Zipping extends MonoOpticCompanion(PZipping)
