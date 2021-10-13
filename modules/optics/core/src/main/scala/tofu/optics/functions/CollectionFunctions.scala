package tofu.optics.functions

import cats.Monoid
import tofu.optics.{Contains, Folded, Items, PFolded, PItems, Property}
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.vector._
import alleycats.std.iterable._
import alleycats.std.map._

import tofu.compat._
import tofu.compat.lazySeqInstances._

trait CollectionFunctions {
  def listElemsP[A, B]: PItems[List[A], List[B], A, B] = PItems.fromTraverse[List, A, B]
  def listElems[A]: Items[List[A], A]                  = listElemsP

  def vecElemsP[A, B]: PItems[Vector[A], Vector[B], A, B] = PItems.fromTraverse[Vector, A, B]
  def vecElems[A, B]: Items[Vector[A], A]                 = vecElemsP

  def streamElemsP[A, B]: PItems[LazySeq[A], LazySeq[B], A, B] = PItems.fromTraverse[LazySeq, A, B]
  def streamElems[A]: Items[LazySeq[A], A]                     = streamElemsP

  def mapValuesP[K, V1, V2]: PItems[Map[K, V1], Map[K, V2], V1, V2] = PItems.fromTraverse[Map[K, +*], V1, V2]
  def mapValues[K, V]: Items[Map[K, V], V]                          = mapValuesP

  def mapItemsP[K, V, A, B]: PFolded[Map[K, V], A, (K, V), B] =
    new PFolded[Map[K, V], A, (K, V), B] {
      override def foldMap[X: Monoid](a: Map[K, V])(f: ((K, V)) => X): X = (a: Iterable[(K, V)]).foldMap(f)

      override def getAll(s: Map[K, V]): List[(K, V)] = s.toList
    }

  def mapItems[K, V]: Folded[Map[K, V], (K, V)] = mapItemsP

  def setAt[A](a: A): Contains[Set[A], Boolean] = Contains[Set[A]](_(a))((s, b) => if (b) s + a else s - a)

  def listAt[A](i: Int): Contains[List[A], Option[A]] = Contains[List[A]](_.lift(i)) {
    case (l, None)    => l
    case (l, Some(a)) => l.updated(i, a)
  }

  def mapAt[K, V](k: K): Contains[Map[K, V], Option[V]] = Contains[Map[K, V]](_.get(k)) {
    case (l, None)    => l - k
    case (l, Some(a)) => l.updated(k, a)
  }

  def listItem[A](i: Int) = Property[List[A]](_.lift(i))(_.updated(i, _))

  def mapItem[K, V](k: K): Property[Map[K, V], V] = Property[Map[K, V]](_.get(k))(_.updated(k, _))

  def vecItem[A](i: Int): Property[Vector[A], A] = Property[Vector[A]](_.lift(i))(_.updated(i, _))
}
