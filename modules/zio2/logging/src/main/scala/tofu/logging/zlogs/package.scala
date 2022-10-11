package tofu.logging

import zio._

package object zlogs {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ULogs       = Logs[UIO, UIO]
  type ZLogging[R] = Logging[URIO[R, *]]
  type ULogging    = Logging[UIO]
}
