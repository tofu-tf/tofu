package tofu.optics.classes

import cats.arrow.Profunctor
import cats.syntax.either._

/** non-category version of cats.arrow.choice */
trait PChoice[P[-_, +_]] extends Profunctor[P] {
  def left[A, B, C](pab: P[A, B]): P[Either[A, C], Either[B, C]]
  def right[A, B, C](pab: P[A, B]): P[Either[C, A], Either[C, B]]
  def optional[A, B, C](pab: P[A, B]): P[Option[A], Option[B]] =
    dimap(right[A, B, Unit](pab))(Either.fromOption(_: Option[A], ()))(
      _.toOption
    )
}

object PChoice {
  def apply[P[-_, +_]](implicit P: PChoice[P]): PChoice[P] = P

  implicit val functionInstance: PChoice[-* => +*] = new PChoice[-* => +*] {
    def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C]        =
      _.leftMap(pab)
    def right[A, B, C](pab: A => B): Either[C, A] => Either[C, B]       = _.map(pab)
    def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D    =
      f andThen fab andThen g
    override def optional[A, B, C](pab: A => B): Option[A] => Option[B] =
      _.map(pab)
  }
}
