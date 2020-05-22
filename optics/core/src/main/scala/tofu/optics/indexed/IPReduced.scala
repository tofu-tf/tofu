package tofu.optics
package indexed

import cats.Monoid
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.kernel.{Order, Semigroup}

trait IPReduced[+I, -S, +T, +A, -B] extends IPFolded[I, S, T, A, B] with PReduced[S, T, A, B] {
  def ireduceMap[X: Semigroup](s: S)(f: (I, A) => X): X

  override def reduceMap[X: Semigroup](s: S)(f: A => X): X = ireduceMap(s)((_, a) => f(a))

  override def ifoldMap[X: Monoid](s: S)(f: (I, A) => X): X = ireduceMap(s)(f)
  def getAll1Index(s: S): NonEmptyList[(I, A)]     = ireduceMap(s)((i, a) => NonEmptyList.one((i, a)))
}
