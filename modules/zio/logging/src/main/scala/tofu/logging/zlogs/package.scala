package tofu.logging

import cats.Id
import zio.{Has, UIO, URIO}
import tofu.higherKind.Embed
import zio.ZIO
import zio.interop.catz._

import scala.reflect.ClassTag

package object zlogs {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ZLogging[R] = Logging[URIO[R, *]]

  type ZLog[R] = Has[ZLogging[R]]
  type ULog    = Has[ZLogging[Any]]

  type TofuLogging = Has[Logging[UIO]]
  type TofuLogs    = Has[Logging.Make[UIO]]

  val TofuLogging: Logging[URIO[TofuLogging, *]] =
    Embed[Logging].embed(ZIO.access[TofuLogging](_.get.widen))

  val TofuLogs: Logging.Make[URIO[TofuLogs, *]] =
    Embed[Logging.Make].embed(ZIO.access[TofuLogs](_.get.biwiden))

  object implicits {
    implicit def tofuLogImplicit[R <: TofuLogging, E]: Logging[ZIO[R, E, *]]                                      = TofuLogging.widen
    implicit def tofuLogsImplicit[R <: TofuLogs, E]: Logging.Make[ZIO[R, E, *]]                                   = TofuLogs.biwiden[Id, ZIO[R, E, *]]
  }
}
