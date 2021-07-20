package tofu

package object common {
  type TimeData[A]    = tofu.time.TimeData[A]
  type TimeZone[F[_]] = tofu.time.TimeZone[F]

  val TimeData: tofu.time.TimeData.type = tofu.time.TimeData
  val TimeZone: tofu.time.TimeZone.type = tofu.time.TimeZone
}
