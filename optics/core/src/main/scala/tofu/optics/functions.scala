package tofu.optics

import cats.Apply
import cats.syntax.apply._
import cats.instances.all._
import alleycats.std.map._

object functions {
  def both2[A, B] = new PRepeated[(A, A), (B, B), A, B] {
    def traverse1[F[+_]: Apply](s: (A, A))(f: A => F[B]): F[(B, B)] =
      (f(s._1), f(s._2)).tupled
  }

  def listElems[A, B]   = PItems.fromTraverse[List, A, B]
  def vectorElems[A, B] = PItems.fromTraverse[Vector, A, B]
  def streamElems[A, B] = PItems.fromTraverse[Stream, A, B]

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

  def mapItems[K, V1, V2]: PItems[Map[K, V1], Map[K, V2], V1, V2] = PItems.fromTraverse[Map[K, +*], V1, V2]

  def listItems[A, B]: PItems[List[A], List[B], A, B] = PItems.fromTraverse[List, A, B]

  def vecItems[A, B]: PItems[Vector[A], Vector[B], A, B] = PItems.fromTraverse[Vector, A, B]

  def everyTuple2[A, B]: PItems[(A, A), (B, B), A, B] =
    PItems[(A, A), A, B](implicit A => (s, f) => (f(s._1), f(s._2)).tupled)

  def everyTuple3[A, B]: PItems[(A, A, A), (B, B, B), A, B] =
    PItems[(A, A, A), A, B](implicit A => (s, f) => (f(s._1), f(s._2), f(s._3)).tupled)

  def everyTuple4[A, B]: PItems[(A, A, A, A), (B, B, B, B), A, B] =
    PItems[(A, A, A, A), A, B](implicit A => (s, f) => (f(s._1), f(s._2), f(s._3), f(s._4)).tupled)
}
