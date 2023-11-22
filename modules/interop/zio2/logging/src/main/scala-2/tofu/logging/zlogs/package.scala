package tofu.logging

import zio.{UIO, URIO}

package object zlogs {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ULogs       = Logs[UIO, UIO]
  type ZLogging[R] = Logging[URIO[R, *]]
  type ULogging    = Logging[UIO]
}
