package tofu.logging

import zio.{Has, UIO, URIO}
import tofu.higherKind.Embed
import zio.ZIO
import zio.interop.catz._

package object zlogs {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ZLogging[R] = Logging[URIO[R, *]]

  type ZLog[R] = Has[ZLogging[R]]
  type ULog    = Has[ZLogging[Any]]

  type TofuLogging = Has[Logging[UIO]]
  type TofuLogs    = Has[Logs.Universal[UIO]]

  val TofuLogging: Logging[URIO[TofuLogging, *]] =
    Embed[Logging].embed(ZIO.access[TofuLogging](_.get.widen))

  val TofuLogs: Logs.Universal[URIO[TofuLogs, *]] =
    Embed[Logs.Universal].embed(ZIO.access[TofuLogs](_.get.biwiden))

  object implicits {
    implicit def tofuLogImplicit[R <: TofuLogging, E]: Logging[ZIO[R, E, *]]      = TofuLogging.widen
    implicit def tofuLogsImplicit[R <: TofuLogs, E]: Logs.Universal[ZIO[R, E, *]] = TofuLogs.biwiden
  }
}
