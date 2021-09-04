package tofu.time

import java.time._
import cats.Monad
import scala.annotation.tailrec

/** typeclass for the types describing some time moment information possibly respecting timezone
  */
trait TimeData[A] {

  /** construct an instance from moment in time and time zone */
  def fromInstant(instant: Instant, zoneId: ZoneId): A

  final def map[B](f: A => B): TimeData[B] = (instant, zoneId) => f(fromInstant(instant, zoneId))
}

object TimeData {

  def apply[A](implicit td: TimeData[A]): TimeData[A] = td

  implicit val timeDataMonad: Monad[TimeData] = new Monad[TimeData] {
    def pure[A](x: A): TimeData[A] = (_, _) => x

    override def map[A, B](fa: TimeData[A])(f: A => B): TimeData[B] = fa.map(f)

    def flatMap[A, B](fa: TimeData[A])(f: A => TimeData[B]): TimeData[B] =
      (instant, zoneId) => f(fa.fromInstant(instant, zoneId)).fromInstant(instant, zoneId)

    def tailRecM[A, B](a: A)(f: A => TimeData[Either[A, B]]): TimeData[B] =
      (instant, zoneId) => {
        @tailrec def go(a: A): B = f(a).fromInstant(instant, zoneId) match {
          case Left(a)  => go(a)
          case Right(b) => b
        }
        go(a)
      }
  }

  implicit val instant: TimeData[Instant]               = (inst, _) => inst
  implicit val zonedDateTime: TimeData[ZonedDateTime]   = ZonedDateTime.ofInstant(_, _)
  implicit val localDateTime: TimeData[LocalDateTime]   = LocalDateTime.ofInstant(_, _)
  implicit val localDate: TimeData[LocalDate]           = localDateTime.map(_.toLocalDate)
  implicit val localTime: TimeData[LocalTime]           = localDateTime.map(_.toLocalTime)
  implicit val offsetDateTime: TimeData[OffsetDateTime] = OffsetDateTime.ofInstant(_, _)
  implicit val offsetTime: TimeData[OffsetTime]         = OffsetTime.ofInstant(_, _)
  implicit val month: TimeData[Month]                   = zonedDateTime.map(_.getMonth)
  implicit val monthDay: TimeData[MonthDay]             = zonedDateTime.map(zdt => MonthDay.of(zdt.getMonth, zdt.getDayOfMonth))
  implicit val dayOfWeek: TimeData[DayOfWeek]           = zonedDateTime.map(_.getDayOfWeek)
  implicit val year: TimeData[Year]                     = zonedDateTime.map(zdt => Year.of(zdt.getYear))
}
