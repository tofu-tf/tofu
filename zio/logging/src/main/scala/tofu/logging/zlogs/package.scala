package tofu.logging

import zio.{Has, UIO, URIO}

package object zlogs {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ZLogging[R] = Logging[URIO[R, *]]

  type ZLog[R] = Has[ZLogging[R]]
  type ULog    = Has[ZLogging[Any]]
}
