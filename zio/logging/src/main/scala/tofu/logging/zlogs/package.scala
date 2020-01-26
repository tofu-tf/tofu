package tofu.logging

import zio.{UIO, URIO}

package object zlogs {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ZLogging[R] = Logging[URIO[R, *]]
}
