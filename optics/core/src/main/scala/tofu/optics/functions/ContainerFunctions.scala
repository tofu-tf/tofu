package tofu.optics.functions

import tofu.optics.Subset
import cats.syntax.either._

trait ContainerFunctions {
  def right[A, B]: Subset[Either[A, B], B] = Subset[Either[A, B]](_.toOption)(_.asRight)

  def left[A, B]: Subset[Either[A, B], A] = Subset[Either[A, B]](_.fold(Some(_), _ => None))(_.asLeft)

  def some[A]: Subset[Option[A], A] = Subset[Option[A]](identity)(Some(_))

  def none[A]: Subset[Option[A], Unit] = Subset[Option[A]](_ => Some(()))(_ => None)
}
