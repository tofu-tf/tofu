package tofu.data

import cats.{Eval, Foldable, Monad, MonoidK}
import tofu.internal.Newtype1Covariant
import cats.syntax.foldable._
import scala.collection.compat._

object PArray extends Newtype1Covariant {
  private def fromArray[X, A](xs: Array[X]): Type[A] = xs.asInstanceOf[Type[A]]

  val empty: PArray[Nothing] = fromArray(Array.empty[Any])

  def apply[A](xs: A*): Type[A] = fromArray((xs: Seq[Any]).toArray)

  def fromFoldable[F[_]: Foldable, A](fa: F[A]): PArray[A] =
    fromArray(fa.foldLeft(Array.newBuilder[Any])((bldr, a) => { bldr += a }).result())

  def fromColl[A](xs: IterableOnce[A]): PArray[A] = fromArray((xs: IterableOnce[Any]).iterator.toArray)

  implicit final class ArrOps[A](private val xs: Type[A]) extends AnyVal {
    private[PArray] def toArray: Array[A] = xs.asInstanceOf[Array[A]]

    def apply(i: Int): A                     = xs.toArray(i)
    def get(i: Int): Option[A]               = xs.toArray.lift(i)
    def length: Int                          = xs.toArray.length
    def slice(from: Int, to: Int): PArray[A] = fromArray(xs.toArray.slice(from, to))
  }

  implicit val arrInstance: ArrInstance = new ArrInstance

  class ArrInstance extends Foldable[PArray] with Monad[PArray] with MonoidK[PArray] {
    def foldLeft[A, B](fa: PArray[A], b: B)(f: (B, A) => B): B                           = fa.toArray.foldLeft(b)(f)
    def foldRight[A, B](fa: PArray[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      val size                = fa.length
      def go(i: Int): Eval[B] =
        if (i == size) lb
        else Eval.defer(f(fa(i), go(i + 1)))
      go(0)
    }
    override def get[A](fa: PArray[A])(idx: Long): Option[A]                             = fa.get(idx.toInt)
    override def map[A, B](fa: PArray[A])(f: A => B): PArray[B]                          = fromArray(fa.toArray.map(f: A => Any))
    def pure[A](x: A): PArray[A]                                                         = PArray(x)
    def flatMap[A, B](fa: PArray[A])(f: A => PArray[B]): PArray[B]                       =
      fromArray(fa.toArray.flatMap(a => (f(a).toArray: Iterable[Any])))

    def tailRecM[A, B](a: A)(f: A => PArray[Either[A, B]]): PArray[B] = {
      val bldr                                                                                   = Array.newBuilder[Any]
      def go(part: PArray[Either[A, B]], i: Int, parts: List[(PArray[Either[A, B]], Int)]): Unit =
        if (i >= part.length) parts match {
          case (first, j) :: rest => go(first, j, rest)
          case Nil                =>
        }
        else
          part(i) match {
            case Right(b) =>
              bldr += b
              go(part, i + 1, parts)
            case Left(a1) =>
              go(f(a1), 0, (part, i + 1) :: parts)
          }
      go(f(a), 0, Nil)
      fromArray(bldr.result())
    }
    def empty[A]: PArray[A]                                           = PArray.empty
    def combineK[A](x: PArray[A], y: PArray[A]): PArray[A]            = PArray.fromArray[Any, A](x.toArray ++ y.toArray)
  }
}
