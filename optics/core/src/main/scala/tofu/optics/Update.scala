package tofu.optics

import cats.{Applicative, Functor}
import tofu.optics.data.Identity

/** aka Setter
  * can update all occurrences of A in S */
trait PUpdate[-S, +T, +A, -B] extends PBase[S, T, A, B] {
  def update(s: S, fb: A => B): T

  def put(s: S, b: B) = update(s, _ => b)
}

object Update extends MonoOpticCompanion(PUpdate)

object PUpdate extends OpticCompanion[PUpdate] {
  def compose[S, T, A, B, U, V](f: PUpdate[A, B, U, V], g: PUpdate[S, T, A, B]): PUpdate[S, T, U, V] =
    (s, fuv) => g.update(s, f.update(_, fuv))

  class Context extends PItems.Context {
    val functor = Applicative[Identity]
    type F[+x] = x
  }

  override def toGeneric[S, T, A, B](o: PUpdate[S, T, A, B]): Optic[Context, S, T, A, B] =
    new Optic[Context, S, T, A, B] {
      def apply(c: Context)(p: A => B): S => T = s => o.update(s, p)
    }

  override def fromGeneric[S, T, A, B](o: Optic[Context, S, T, A, B]): PUpdate[S, T, A, B] =
    (a, fab) => o(new Context)(fab)(a)

  final implicit def byFunctor[F[_], A, B](implicit F: Functor[F]): PUpdate[F[A], F[B], A, B] = F.map(_)(_)
}
