package tofu.optics

import alleycats.Pure
import cats.{Functor, Id}
import tofu.optics.compat.unused212
import tofu.optics.classes.PChoice
import tofu.optics.data._

trait PUpcast[-S, +T, +A, -B] extends PBase[PUpcast, S, T, A, B] {
  def upcast(b: B): T
}

object Upcast extends MonoOpticCompanion(PUpcast)

object PUpcast extends OpticCompanion[PUpcast] with OpticProduct[PUpcast] {

  def compose[S, T, A, B, U, V](f: PUpcast[A, B, U, V], g: PUpcast[S, T, A, B]): PUpcast[S, T, U, V] =
    new PComposed[PUpcast, S, T, A, B, U, V](g, f) with PUpcast[S, T, U, V] {
      def upcast(v: V): T = g.upcast(f.upcast(v))
    }

  override def product[S1, S2, T1, T2, A1, A2, B1, B2](
      f: PUpcast[S1, T1, A1, B1],
      g: PUpcast[S2, T2, A2, B2]
  ) = { case (b1, b2) => (f.upcast(b1), g.upcast(b2)) }

  class Context extends PZipping.Context with PSubset.Context {
    override type P[-x, +y] = Tagged[x, y]
    type G[+x]              = Unit
    def pure                 = Pure[Id]
    override def profunctor  = PChoice[Tagged]
    def gfunctor: Functor[G] = ProxyFunctor
  }
  def toGeneric[S, T, A, B](o: PUpcast[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: Tagged[A, B]): Tagged[S, T] = _ => o.upcast(p(()))
    }
  def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PUpcast[S, T, A, B] =
    b => o(new Context)(_ => b)(())

  object GenericSubtypeImpl extends Upcast[Any, Any] {
    override def upcast(b: Any): Any = b
  }

  implicit def subType[E, E1](implicit @unused212 ev: E <:< E1): Upcast[E1, E] =
    GenericSubtypeImpl.asInstanceOf[Upcast[E1, E]]

  override def delayed[S, T, A, B](o: () => PUpcast[S, T, A, B]): PUpcast[S, T, A, B] = new PUpcast[S, T, A, B] {
    lazy val opt        = o()
    def upcast(b: B): T = opt.upcast(b)
  }
}
