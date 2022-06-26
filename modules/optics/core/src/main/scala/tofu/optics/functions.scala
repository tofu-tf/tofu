package tofu.optics

import alleycats.std.iterable._
import alleycats.std.map._
import cats.Apply
import cats.Monoid
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.vector._

import tofu.optics.compat.LazySeq
import tofu.optics.compat.lazySeqInstances._

object functions extends CollectionFunctions with TupleFunctions with ContainerFunctions {
  def extractSubtype[A <: B, B]: Extract[A, B] = (s: B) => s

  def filter[A](p: A => Boolean): Subset[A, A] = new Subset[A, A] {
    override def narrow(a: A): Either[A, A] = if (p(a)) Right(a) else Left(a)

    override def upcast(b: A): A = b
  }
}

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

trait TupleFunctions {
  def both2[A]: Repeated[(A, A), A]         = everyTuple2
  def allThree[A]: Repeated[(A, A, A), A]   = everyTuple3
  def allFour[A]: Repeated[(A, A, A, A), A] = everyTuple4

  def everyTuple2[A, B]: PRepeated[(A, A), (B, B), A, B] =
    new PRepeated[(A, A), (B, B), A, B] {
      override def traverse1[F[+_]: Apply](s: (A, A))(f: A => F[B]): F[(B, B)] = (f(s._1), f(s._2)).tupled
    }

  def everyTuple3[A, B]: PRepeated[(A, A, A), (B, B, B), A, B] = new PRepeated[(A, A, A), (B, B, B), A, B] {
    override def traverse1[F[+_]: Apply](s: (A, A, A))(f: A => F[B]): F[(B, B, B)] =
      (f(s._1), f(s._2), f(s._3)).tupled
  }

  def everyTuple4[A, B]: PRepeated[(A, A, A, A), (B, B, B, B), A, B] = new PRepeated[(A, A, A, A), (B, B, B, B), A, B] {
    override def traverse1[F[+_]: Apply](s: (A, A, A, A))(f: A => F[B]): F[(B, B, B, B)] =
      (f(s._1), f(s._2), f(s._3), f(s._4)).tupled
  }

  def firstP[A, B, A1]: PContains[(A, B), (A1, B), A, A1] = new PContains[(A, B), (A1, B), A, A1] {
    def set(s: (A, B), b: A1): (A1, B) = (b, s._2)

    def extract(s: (A, B)): A = s._1
  }

  def first[A, B]: Contains[(A, B), A] = firstP

  def secondP[A, B, B1]: PContains[(A, B), (A, B1), B, B1] = new PContains[(A, B), (A, B1), B, B1] {
    def set(s: (A, B), b: B1): (A, B1) = (s._1, b)

    def extract(s: (A, B)): B = s._2
  }

  def second[A, B]: Contains[(A, B), B] = secondP

  def containsUnit[A, B >: Unit]: Contains[A, B] = new Contains[A, B] {
    def extract(s: A): B   = ()
    def set(s: A, b: B): A = s
  }

  def unit[A]: Contains[A, Unit] = containsUnit
  def any[A]: Contains[A, Any]   = containsUnit
}

trait ContainerFunctions {
  def right[A, B]: Subset[Either[A, B], B] = Subset[Either[A, B]](_.toOption)(_.asRight)

  def left[A, B]: Subset[Either[A, B], A] = Subset[Either[A, B]](_.fold(Some(_), _ => None))(_.asLeft)

  def some[A]: Subset[Option[A], A] = Subset[Option[A]](identity)(Some(_))

  def none[A]: Subset[Option[A], Unit] = Subset[Option[A]](_ => Some(()))(_ => None)
}
