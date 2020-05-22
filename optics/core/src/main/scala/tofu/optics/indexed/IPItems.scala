package tofu.optics
package indexed

import cats.{Applicative, Monoid}
import tofu.optics.data.{Constant, Identity}

trait IPItems[+I, -S, +T, +A, -B] extends PItems[S, T, A, B] with IPUpdate[I, S, T, A, B] with IPFolded[I, S, T, A, B]  {
  def itraverse[F[+_]: Applicative](s: S)(f: (I, A) => F[B]): F[T]

  override def iupdate(a: S, fb: (I, A) => B): T       = itraverse[Identity](a)(fb)
  override def ifoldMap[X: Monoid](a: S)(f: (I, A) => X): X =
    itraverse[Constant[X, +*]](a)((i, b) => Constant(f(i, b))).value

  override def traverse[F[+_]: Applicative](s: S)(f: A => F[B]): F[T] = itraverse(s)((_, a) => f(a))
}
