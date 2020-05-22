package tofu.optics.indexed

import cats.Monoid
import tofu.optics.PFolded
import cats.instances.list._
import cats.instances.vector._
import cats.instances.map._
import cats.kernel.Semigroup

trait IPFolded[+I, -S, +T, +A, -B] extends PFolded[S, T, A, B] {
  def ifoldMap[X: Monoid](s: S)(f: (I, A) => X): X

  def groupBy[I1 >: I, X: Semigroup](s: S)(f: (I, A) => X): Map[I1, X] = ifoldMap(s)((i, a) => Map((i, f(i, a))))
  def getListMap[I1 >: I](s: S): Map[I1, List[A]]                      = groupBy[I1, List[A]](s)((_, a) => List(a))
  def getAllIndex(s: S): List[(I, A)]                                  = ifoldMap(s)((i, a) => List((i, a)))
  def toVectorIndex(s: S): Vector[(I, A)]                              = ifoldMap(s)((i, a) => Vector((i, a)))

  override def foldMap[X: Monoid](s: S)(f: A => X): X = ifoldMap(s)((_, a) => f(a))
}
