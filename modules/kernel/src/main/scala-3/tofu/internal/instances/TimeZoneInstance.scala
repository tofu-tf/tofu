package tofu.internal.instances

import cats.~>
import tofu.higherKind.{RepK, RepresentableK}
import tofu.time.TimeZone

import java.time.{ZoneId, ZoneOffset}

trait TimeZoneInstance {
  // TODO: use higherKind.derived macro when it is ready for scala 3
  given timeZoneRepresentableK: RepresentableK[TimeZone] = new RepresentableK[TimeZone] {
    def tabulate[F[_]](hom: RepK[TimeZone, _] ~> F): TimeZone[F] = new TimeZone[F] {
      override def system: F[ZoneId]                                       = hom(RepK[TimeZone](_.system))
      override def available: F[Set[String]]                               = hom(RepK[TimeZone](_.available))
      override def of(zoneId: String): F[ZoneId]                           = hom(RepK[TimeZone](_.of(zoneId)))
      override def ofOffset(prefix: String, offset: ZoneOffset): F[ZoneId] =
        hom(RepK[TimeZone](_.ofOffset(prefix, offset)))
    }
  }

}
