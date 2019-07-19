package tofu.optics.data

import cats.arrow.Compose
import cats.syntax.either._
import cats.syntax.option._
import tofu.optics.classes.PChoice
import tofu.optics.classes.PChoice

/** reversed version of cats.data.Const */
final case class Tagged[-A, +B](value: B) extends AnyVal {
  def map[C](f: B => C): Tagged[A, C] = Tagged(f(value))
  def retag[C]: Tagged[C, B]          = Tagged(value)
}

object Tagged {
  implicit val profunctor: Compose[Tagged] with PChoice[Tagged] =
    new Compose[Tagged] with PChoice[Tagged] {
      def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A)(
          g: B => D
      ): Tagged[C, D] = fab.map(g).retag
      def compose[A, B, C](f: Tagged[B, C], g: Tagged[A, B]): Tagged[A, C] =
        f.retag
      def left[A, B, C](pab: Tagged[A, B]): Tagged[Either[A, C], Either[B, C]] =
        pab.map(_.asLeft[C]).retag
      def right[A, B, C](
          pab: Tagged[A, B]
      ): Tagged[Either[C, A], Either[C, B]] = pab.map(_.asRight[C]).retag
      override def optional[A, B, C](
          pab: Tagged[A, B]
      ): Tagged[Option[A], Option[B]] = pab.map(_.some).retag
    }
}
