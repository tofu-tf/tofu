package tofu.optics
package indexed

import cats.kernel.Semigroup
import cats.{Applicative, Apply}
import tofu.optics.data.Constant

trait IPRepeated[+I, -S, +T, +A, -B]
    extends PRepeated[S, T, A, B] with IPItems[I, S, T, A, B] with IPReduced[I, S, T, A, B] {
  def itraverse1[F[+_]: Apply](s: S)(f: (I, A) => F[B]): F[T]

  def itraverse[F[+_]: Applicative](a: S)(f: (I, A) => F[B]): F[T] = itraverse1(a)(f)

  def traverse1[F[+_]: Apply](s: S)(f: A => F[B]): F[T] =
    itraverse1(s)((_, a) => f(a))

  def ireduceMap[X: Semigroup](s: S)(f: (I, A) => X): X =
    itraverse1[Constant[X, +*]](s)((i, b) => Constant(f(i, b))).value
}
