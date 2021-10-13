package tofu.optics

import cats.Monoid
import cats.instances.option._
import cats.syntax.foldable._
import tofu.optics.data.Constant

import scala.reflect.{ClassTag, classTag}

/** S could be T or not partial function from S to T and part of prism
  */
trait PDowncast[-S, +T, +A, -B] extends PFolded[S, T, A, B] with PBase[PDowncast, S, T, A, B] {
  def downcast(s: S): Option[A]

  override def as[B1, T1]: PDowncast[S, T1, A, B1] = this.asInstanceOf[PDowncast[S, T1, A, B1]]

  def getOption(s: S): Option[A] = downcast(s)

  def foldMap[X: Monoid](s: S)(f: A => X): X = downcast(s).foldMap(f)
}

object Downcast extends MonoOpticCompanion(PDowncast)

object PDowncast extends OpticCompanion[PDowncast] {

  def compose[S, T, A, B, U, V](f: PDowncast[A, B, U, V], g: PDowncast[S, T, A, B]): PDowncast[S, T, U, V] =
    new PComposed[PDowncast, S, T, A, B, U, V](g, f) with PDowncast[S, T, U, V] {
      def downcast(s: S): Option[U] = g.downcast(s).flatMap(f.downcast)
    }

  trait Context extends PExtract.Context {
    def default: X
  }

  override def toGeneric[S, T, A, B](o: PDowncast[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => Constant[c.X, B]): S => Constant[c.X, T] =
        s => o.downcast(s).fold(c.default)(a => p(a))
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PDowncast[S, T, A, B] =
    s =>
      o(new Context {
        type X = Option[A]
        def default: Option[A] = None
      })(a => Some(a))(s)

  implicit def subType[A, B <: A: ClassTag]: Downcast[A, B] =
    new Downcast[A, B] {
      def downcast(s: A): Option[B] = Some(s).collect { case b: B => b }

      override def toString: String = {
        val name = classTag[B].runtimeClass.getCanonicalName.split("\\.").last
        s":$name"
      }
    }

  override def delayed[S, T, A, B](o: () => PDowncast[S, T, A, B]): PDowncast[S, T, A, B] = new PDowncast[S, T, A, B] {
    lazy val opt                  = o()
    def downcast(s: S): Option[A] = opt.downcast(s)
  }
}
