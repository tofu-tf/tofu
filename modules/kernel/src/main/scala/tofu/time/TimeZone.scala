package tofu.time

import cats.data.*
import cats.{Applicative, FlatMap, Functor, Monoid}
import tofu.Delay
import tofu.internal.instances.TimeZoneInstance
import tofu.syntax.liftKernel.CatsTaglessLiftSyntax

import java.time.*
import scala.jdk.CollectionConverters.*

/** wrapping around ZoneId methods
  */
trait TimeZone[F[_]] {

  /** current System time zone */
  def system: F[ZoneId]

  /** list of available time zones */
  def available: F[Set[String]]

  /** get ZoneId by full string ID, checking for availability, probably generating an error see documentation to
    * `java.time.ZoneId.of`
    */
  def of(zoneId: String): F[ZoneId]

  /** Obtains an instance of {@code ZoneId} wrapping an offset. <p> If the prefix is "GMT", "UTC", or "UT" a {@code
    * ZoneId} with the prefix and the non-zero offset is returned. If the prefix is empty {@code ""} the {@code
    * ZoneOffset} is returned.
    */
  def ofOffset(prefix: String, offset: ZoneOffset): F[ZoneId]
}

object TimeZone extends TimeZoneInstances {

  def apply[F[_]](implicit tz: TimeZone[F]): TimeZone[F] = tz

  implicit def timeZoneForKleisli[F[_]: TimeZone, R]: TimeZone[Kleisli[F, R, _]]                            = TimeZone[F].lift
  implicit def timeZoneForWriterT[F[_]: Applicative: TimeZone, R: Monoid]: TimeZone[WriterT[F, R, _]]       = TimeZone[F].lift
  implicit def timeZoneForOptionT[F[_]: Functor: TimeZone]: TimeZone[OptionT[F, _]]                         = TimeZone[F].lift
  implicit def timeZoneForEitherT[F[_]: Functor: TimeZone, E]: TimeZone[EitherT[F, E, _]]                   = TimeZone[F].lift
  implicit def timeZoneForStateT[F[_]: Applicative: TimeZone, S]: TimeZone[StateT[F, S, _]]                 = TimeZone[F].lift
  implicit def timeZoneForIorT[F[_]: Applicative: TimeZone, L]: TimeZone[IorT[F, L, _]]                     = TimeZone[F].lift
  implicit def timeZoneForContT[F[_]: FlatMap: TimeZone, R]: TimeZone[ContT[F, R, _]]                       = TimeZone[F].lift
  implicit def timeZoneForRWST[F[_]: Applicative: TimeZone, R, L: Monoid, S]: TimeZone[RWST[F, R, L, S, _]] =
    TimeZone[F].lift
}

private[tofu] trait TimeZoneInstances extends TimeZoneInstance {
  implicit def syncSystem[F[_]](implicit F: Delay[F]): TimeZone[F] = new TimeZone[F] {
    def system: F[ZoneId] = F.delay(ZoneId.systemDefault())

    def available: F[Set[String]] = F.delay(ZoneId.getAvailableZoneIds.asScala.toSet)

    def of(zoneId: String): F[ZoneId] = F.delay(ZoneId.of(zoneId))

    def ofOffset(prefix: String, offset: ZoneOffset): F[ZoneId] = F.delay(ZoneId.ofOffset(prefix, offset))
  }
}
