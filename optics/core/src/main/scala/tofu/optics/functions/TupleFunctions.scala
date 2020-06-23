package tofu.optics
package functions
import tofu.syntax.monadic._
import cats.Apply

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
