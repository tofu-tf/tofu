package tofu.optics

import cats.{Applicative, Functor}
import tofu.optics.data.Identity

/** aka Setter can update all occurrences of A in S
  */
trait PUpdate[-S, +T, +A, -B] extends PBase[PUpdate, S, T, A, B] {
  def update(s: S, fb: A => B): T

  def updateF(fb: A => B): S => T = update(_, fb)

  def put(s: S, b: B): T = update(s, _ => b)

  def putF(b: B): S => T = put(_, b)

  def follow[A1 >: A, B1 <: B, U](upd: PUpdate[T, U, A1, B1]): PUpdate[S, U, A1, B1] =
    (s, f) => upd.update(this.update(s, f), f)

  def **[A1 >: A, B1 <: B, U](upd: PUpdate[T, U, A1, B1]): PUpdate[S, U, A1, B1] =
    follow(upd)
}

object Update extends MonoOpticCompanion(PUpdate)

object PUpdate extends OpticCompanion[PUpdate] {
  def compose[S, T, A, B, U, V](f: PUpdate[A, B, U, V], g: PUpdate[S, T, A, B]): PUpdate[S, T, U, V] =
    new PComposed[PUpdate, S, T, A, B, U, V](g, f) with PUpdate[S, T, U, V] {
      def update(s: S, uv: U => V): T = g.update(s, f.update(_, uv))
    }

  override def delayed[S, T, A, B](o: () => PUpdate[S, T, A, B]): PUpdate[S, T, A, B] = new PUpdate[S, T, A, B] {

    override def update(s: S, fb: A => B): T = opt.update(s, fb)

    lazy val opt = o()
  }

  class Context extends PZipping.Context with PItems.Context {
    def gfunctor = Applicative[Identity]
    override type G[+x] = x
  }

  override def toGeneric[S, T, A, B](o: PUpdate[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => B): S => T = s => o.update(s, p)
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PUpdate[S, T, A, B] =
    (a, fab) => o(new Context)(fab)(a)

  final implicit def byFunctor[F[_], A, B](implicit F: Functor[F]): PUpdate[F[A], F[B], A, B] = F.map(_)(_)
}
