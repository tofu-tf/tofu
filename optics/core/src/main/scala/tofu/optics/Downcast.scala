package tofu.optics

import cats.Monoid
import cats.data.Const
import cats.instances.option._
import cats.syntax.foldable._
import tofu.optics.data.Constant

/** S could be T or not
  * partial function from S to T
  * and part of prism
  */
trait PDowncast[-S, +T, +A, -B]  extends PFolded[S, T, A, B] {
  def downcast(s: S): Option[A]

  def foldMap[X: Monoid](s: S)(f: A => X): X = downcast(s).foldMap(f)
}

object Downcast extends MonoOpticCompanion(PDowncast)

object PDowncast extends OpticCompanion[PDowncast] {
  def compose[S, T, A, B, U, V](f: PDowncast[A, B, U, V], g: PDowncast[S, T, A, B]): PDowncast[S, T, U, V] =
    s => g.downcast(s).flatMap(f.downcast)

  trait Context extends PExtract.Context {
    def default: X
  }

  override def toGeneric[S, T, A, B](o: PDowncast[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => Constant[c.X, B]): S => Constant[c.X, T] =
        s => o.downcast(s).fold(Constant.of[T](c.default))(a => p(a).retag)
    }
  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PDowncast[S, T, A, B] =
    s =>
      o(new Context {
        type X = Option[A]
        def default: Option[A] = None
      })(a => Constant(Some(a)))(s).value
}
