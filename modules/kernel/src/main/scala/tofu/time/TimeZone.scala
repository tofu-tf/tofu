package tofu.time

import java.time._

import scala.jdk.CollectionConverters._
import tofu.Delay

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

object TimeZone {
  implicit def syncSystem[F[_]](implicit F: Delay[F]): TimeZone[F] = new TimeZone[F] {
    def system: F[ZoneId] = F.delay(ZoneId.systemDefault())

    def available: F[Set[String]] = F.delay(ZoneId.getAvailableZoneIds.asScala.toSet)

    def of(zoneId: String): F[ZoneId] = F.delay(ZoneId.of(zoneId))

    def ofOffset(prefix: String, offset: ZoneOffset): F[ZoneId] = F.delay(ZoneId.ofOffset(prefix, offset))
  }
}
