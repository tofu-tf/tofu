package tofu.optics

import alleycats.Pure
import cats.{Functor, Id}
import tofu.optics.classes.PChoice
import tofu.optics.data.{Identity, Tagged}

trait PUpcast[-S, +T, +A, -B] extends PBase[S, T, A, B] {
  def upcast(b: B): T
}

object Upcast extends MonoOpticCompanion(PUpcast)

object PUpcast extends OpticCompanion[PUpcast] {

  def compose[S, T, A, B, U, V](f: PUpcast[A, B, U, V], g: PUpcast[S, T, A, B]): PUpcast[S, T, U, V] =
    v => g.upcast(f.upcast(v))

  class Context extends PSubset.Context {
    override type P[-x, +y] = Tagged[x, y]
    type F[+x]              = x
    def pure       = Pure[Id]
    def profunctor = PChoice[Tagged]
    def functor    = Functor[Identity]
  }

  def toGeneric[S, T, A, B](o: PUpcast[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: Tagged[A, B]): Tagged[S, T] = Tagged(o.upcast(p.value))
    }

  def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PUpcast[S, T, A, B] =
    b => o(new Context)(Tagged(b)).value

  object GenericSubtypeImpl extends Upcast[Any, Any] {
    override def upcast(b: Any): Any = b
  }

  implicit def subtype[E, E1](implicit ev: E <:< E1): Upcast[E1, E] =
    GenericSubtypeImpl.asInstanceOf[Upcast[E1, E]]
}
